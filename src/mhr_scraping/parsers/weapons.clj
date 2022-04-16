(ns mhr-scraping.parsers.weapons
  (:require
   [clojure.string :as string]
   [hickory.core :as h]
   [hickory.select :as hs]
   [mhr-scraping.http :as http]))

(defn text-content
  [element]
  (when (and (map? element)
             (:content element))
    (->> (map (fn [e]
                (if (string? e)
                  (string/trim e)
                  (text-content e)))
              (:content element))
         (string/join "\n")
         string/trim)))

(defn get-details
  [url]
  (->> (:body (http/get url))
       h/parse
       h/as-hickory
       (hs/select
        (hs/descendant (hs/tag :table)))
       second))

(defn parse-table-row
  [weapon-type rows]
  (map (fn [row]
         (let [image (->> row
                          (hs/select (hs/descendant
                                      (hs/and (hs/tag :td)
                                              (hs/nth-child 1))
                                      (hs/tag :img)))
                          first
                          (#(get-in % [:attrs :src])))
               weapon-name (->> row
                                (hs/select (hs/descendant
                                            (hs/and (hs/tag :td)
                                                    (hs/nth-child 2))
                                            (hs/tag :div)
                                            (hs/tag :a)))
                                first
                                (text-content))
               decorations (->> row
                                (hs/select (hs/descendant
                                            (hs/and (hs/tag :td)
                                                    (hs/nth-child 2))
                                            (hs/tag :img)))
                                (mapv (fn [el]
                                        (get-in el [:attrs :src]))))
               rampage-skills (->> row
                                   (hs/select (hs/descendant
                                               (hs/and (hs/tag :td)
                                                       (hs/nth-child 3))
                                               (hs/tag :div)))
                                   (mapv text-content))
               raw-damage (some->> row
                                   (hs/select (hs/descendant
                                               (hs/and (hs/tag :td)
                                                       (hs/nth-child 4))
                                               (hs/tag :div)))
                                   first
                                   :content
                                   string/join
                                   string/trim
                                   (re-find #"\d+")
                                   (Long/parseLong))
               defense-bonus (some->> row
                                      (hs/select
                                       (hs/child (hs/and (hs/tag :td)
                                                         (hs/nth-child 5))
                                                 (hs/and (hs/tag :div)
                                                         (hs/find-in-text
                                                          #"Defense "))))
                                      first
                                      text-content
                                      (re-find #"Bonus\s+[\-|\+]?(\d+)")
                                      last
                                      (Long/parseLong))
               affinity (some->> row
                                 (hs/select (hs/child
                                             (hs/and (hs/tag :td)
                                                     (hs/nth-child 5))
                                             (hs/tag :div)
                                             (hs/tag :span)))
                                 last
                                 :content
                                 first
                                 string/join
                                 string/trim
                                 (re-find #"[\-|\+]?\d+")
                                 (Long/parseLong))
               element (->> row
                            (hs/select (hs/child
                                        (hs/and (hs/tag :td)
                                                (hs/nth-child 5))
                                        (hs/and (hs/tag :span)
                                                (hs/has-descendant
                                                 (hs/tag :svg)))))
                            first
                            ((fn [el]
                               (when-let [img
                                          (some-> (hs/select
                                                   (hs/child (hs/tag :img))
                                                   el)
                                                  first
                                                  (get-in [:attrs :src]))]
                                 [img (some->> (text-content el)
                                               (re-find #"\d+")
                                               (Long/parseLong))]))))
               sharpness
               (when-not (contains? #{"Bow"
                                      "Light Bowgun"
                                      "Heavy Bowgun"}
                                    weapon-type)
                 (->> row
                      (hs/select
                       (hs/child (hs/and (hs/tag :td)
                                         (hs/nth-child 6))
                                 (hs/tag :div)
                                 (hs/tag :div)))
                      (map-indexed
                       (fn [idx {divs :content}]
                         (->> divs
                              (mapv (fn [{{:keys [style]} :attrs}]
                                      (let [n (some->> style
                                                       (re-find #"(\d+)px")
                                                       last
                                                       (Long/parseLong))]
                                        (when (and n (not (zero? n)))
                                          [n
                                           (re-find #"#\S{6}" style)]))))
                              (remove nil?)
                              (reduce (fn [accl [n color]]
                                        (conj accl
                                              {:sharpness/type (case idx
                                                                 1 :bonus
                                                                 :base)
                                               :sharpness/value n
                                               :sharpness/color color}))
                                      []))))
                      (apply concat)
                      vec))
               horn-notes (when (= weapon-type "Hunting Horn")
                            (->> row
                                 (hs/select
                                  (hs/descendant (hs/and (hs/tag :td)
                                                         (hs/nth-child 7))
                                                 (hs/tag :div)))
                                 (mapv (fn [el]
                                         (-> el
                                             :content
                                             first
                                             (string/trim))))))
               shelling-type (when (= weapon-type "Gunlance")
                               (some->> row
                                        (hs/select
                                         (hs/descendant (hs/and
                                                         (hs/tag :td)
                                                         (hs/nth-child 7))
                                                        (hs/tag :div)))
                                        first
                                        text-content))
               kinsect-level (when (= weapon-type "Insect Glaive")
                               (some->> row
                                        (hs/select
                                         (hs/descendant (hs/and
                                                         (hs/tag :td)
                                                         (hs/nth-child 7))
                                                        (hs/tag :div)))
                                        first
                                        text-content
                                        (re-find #"\d+")
                                        (Long/parseLong)))
               phial (when (contains? #{"Switch Axe"
                                        "Charge Blade"} weapon-type)
                       (let [s (->> row
                                    (hs/select
                                     (hs/descendant (hs/and (hs/tag :td)
                                                            (hs/nth-child 7))
                                                    (hs/tag :div)))
                                    first
                                    text-content)
                             value (some->> s
                                            (re-find #"\d+")
                                            (Long/parseLong))]
                         (cond-> {:phial/type (-> s
                                                  (string/replace #"\d+" "")
                                                  string/trim)}
                           value (assoc :phial/value value))))
               ;; TODO: Check styles. Are classes important information?
               coatings (when (= weapon-type "Bow")
                          (->> row
                               (hs/select
                                (hs/descendant (hs/and (hs/tag :td)
                                                       (hs/nth-child 7))
                                               (hs/tag :div)
                                               (hs/tag :div)))
                               (mapv text-content)))
               ;; TODO: Check styles. Are classes important information?
               charge-lv-atk (when (= weapon-type "Bow")
                               (->> row
                                    (hs/select
                                     (hs/descendant (hs/and (hs/tag :td)
                                                            (hs/nth-child 6))
                                                    (hs/tag :div)
                                                    (hs/tag :div)))
                                    (mapv text-content)))
               [deviation
                recoil
                reload] (when (contains? #{"Heavy Bowgun"
                                           "Light Bowgun"} weapon-type)
                          (->> row
                               (hs/select
                                (hs/descendant (hs/and (hs/tag :td)
                                                       (hs/nth-child 6))
                                               (hs/tag :div)))
                               (mapv (comp (fn [s]
                                             (->> (string/split s #"\s+")
                                                  rest
                                                  (string/join " ")))
                                           text-content))))
               #_#_details (-> (hs/select
                                (hs/descendant (hs/and (hs/tag :td)
                                                       (hs/nth-child 2))
                                               (hs/tag :div)
                                               (hs/tag :a)) row)
                               first
                               (get-in [:attrs :href])
                               (get-details))]
           ;; Conditionally add weapon attributes if they exist
           (cond-> {:weapon/name weapon-name
                    :weapon/type weapon-type
                    #_#_:weapon/details details}
             (not-empty decorations) (assoc :weapon/decorations decorations)
             (http/exists? image) (assoc :weapon/image image)
             (not-empty rampage-skills) (assoc :weapon/rampage-skills
                                               rampage-skills)
             raw-damage (assoc :weapon/raw-damage raw-damage)
             defense-bonus (assoc :weapon/defense-bonus defense-bonus)
             affinity (assoc :weapon/affinity affinity)
             element (assoc :weapon/element element)
             sharpness (assoc :weapon/sharpness sharpness)
             horn-notes (assoc :weapon/horn-notes horn-notes)
             shelling-type (assoc :weapon/shelling-type shelling-type)
             kinsect-level (assoc :weapon/kinsect-level kinsect-level)
             phial (assoc :weapon/phial phial)
             charge-lv-atk (assoc :weapon/charge-level-atk charge-lv-atk)
             coatings (assoc :weapon/coatings coatings)
             deviation (assoc :weapon/deviation deviation)
             recoil (assoc :weapon/recoil recoil)
             reload (assoc :weapon/reload reload))))
       rows))

(defn weapons
  [body]
  (->> body
       h/parse
       h/as-hickory
       (hs/select
        (hs/descendant (hs/tag :aside)))
       (pmap (fn [element]
               (let [weapon-type (->> element
                                      (hs/select (hs/descendant
                                                  (hs/tag :h3)))
                                      first
                                      :content
                                      (string/join)
                                      (string/trim))]
                 (->> element
                      (hs/select (hs/child
                                  (hs/and (hs/tag :table)
                                          (hs/class "min-w-full"))
                                  (hs/tag :tbody)
                                  (hs/tag :tr)))
                      (parse-table-row weapon-type)))))
       doall
       (apply concat)))

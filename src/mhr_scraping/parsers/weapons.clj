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

(defn get-px-amt
  [value]
  (Long/parseLong (re-find #"\d+" value)))

(defn get-sharpness
  [elements]
  (let [base (->> elements
                  first
                  :content
                  (map (fn [entry]
                         (-> (get-in entry [:attrs :style])
                             (string/split #";|:")
                             second
                             (get-px-amt)
                             float
                             (* 100 (/ 88))))))
        bonus (->> elements
                   second
                   :content
                   (map (fn [entry]
                          (-> (get-in entry [:attrs :style])
                              (string/split #";|:")
                              second
                              (get-px-amt)
                              float
                              (* 100 (/ 88))))))]
    {:base base
     :bonus bonus}))

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
  (->> rows
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
                                                 (hs/tag :div)))
                                     first
                                     (text-content))
                    decorations (->> row
                                     (hs/select (hs/descendant
                                                 (hs/and (hs/tag :td)
                                                         (hs/nth-child 2))
                                                 (hs/tag :img)))
                                     (map (fn [el]
                                            (get-in el [:attrs :src]))))
                    rampage-skills (->> row
                                        (hs/select (hs/descendant
                                                    (hs/and (hs/tag :td)
                                                            (hs/nth-child 3))
                                                    (hs/tag :div)))
                                        (map text-content))
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

                    #_#_details (-> (hs/select
                                     (hs/descendant (hs/and (hs/tag :td)
                                                            (hs/nth-child 2))
                                                    (hs/tag :div)
                                                    (hs/tag :a)) row)
                                    first
                                    (get-in [:attrs :href])
                                    (get-details))

                    #_#_sharpness (when-not (contains? #{"Bow"
                                                         "Light Bowgun"
                                                         "Heavy Bowgun"}
                                                       weapon-type)
                                    (->> row
                                         (hs/select
                                          (hs/child (hs/and (hs/tag :td)
                                                            (hs/nth-child 6))
                                                    (hs/tag :div)))
                                         first
                                         :content
                                         (get-sharpness)))

                    #_#_horn-notes
                    (when (= weapon-type "Hunting Horn")
                      (->> row
                           (hs/select
                            (hs/descendant (hs/and (hs/tag :td)
                                                   (hs/nth-child 7))
                                           (hs/tag :div)))
                           (map (fn [el]
                                  (-> el
                                      :content
                                      first
                                      (string/trim))))))

                    #_#_shelling-type
                    (when (= weapon-type "Gunlance")
                      (->> row
                           (hs/select
                            (hs/descendant (hs/and (hs/tag :td)
                                                   (hs/nth-child 7))
                                           (hs/tag :div)))
                           (map (fn [el]
                                  (-> el
                                      :content
                                      first
                                      (string/trim))))))

                    #_#_phial-type
                    ;;some of these have values?? check if they can be left out
                    (when (contains? #{"Switch Axe"
                                       "Charge Blade"} weapon-key)
                      (->> row
                           (hs/select
                            (hs/descendant (hs/and (hs/tag :td)
                                                   (hs/nth-child 7))
                                           (hs/tag :div)))
                           (map (fn [el]
                                  (-> el
                                      :content
                                      first
                                      (string/trim))))))

                    #_#_kinsect-lv
                    (when (= weapon-type "Insect Glaive")
                      (->> row
                           (hs/select
                            (hs/descendant (hs/and (hs/tag :td)
                                                   (hs/nth-child 7))
                                           (hs/tag :div)))
                           (map (fn [el]
                                  (-> el
                                      :content
                                      first
                                      (string/trim))))))

                    #_#_charge-lv-atk
                    ;; last item is greyed out... is green or grey class required info?
                    (when (= weapon-type "Bow")
                      (->> row
                           (hs/select
                            (hs/descendant (hs/and (hs/tag :td)
                                                   (hs/nth-child 6))
                                           (hs/tag :div)))
                           (map (fn [el]
                                  (-> el
                                      :content
                                      first)))))

                    #_#_coatings
                    ;; check for grey in class?? is green or grey class required info?
                    (when (= weapon-type "Bow")
                      (->> row
                           (hs/select
                            (hs/descendant (hs/and (hs/tag :td)
                                                   (hs/nth-child 7))
                                           (hs/tag :div)))
                           (map (fn [el]
                                  (-> el
                                      :content
                                      first
                                      (string/trim))))))

                    #_drr
                    #_(when (contains? #{"Heavy Bowgun"
                                         "Light Bowgun"} weapon-type)
                        (->> row
                             (hs/select
                              (hs/descendant (hs/and (hs/tag :td)
                                                     (hs/nth-child 6))
                                             (hs/tag :div)))
                             (map (fn [el]
                                    (-> el
                                        :content
                                        first
                                        (string/trim)
                                        (string/split #" " 2))))))]
                ;; Conditionally add weapon attributes if they exist
                (cond-> {:weapon/name weapon-name
                         :weapon/type weapon-type
                         ;; :weapon/details details
                         }
                  (not-empty decorations)
                  (assoc :weapon/decorations decorations)
                  (http/exists? image)
                  (assoc :weapon/image image)
                  (not-empty rampage-skills)
                  (assoc :weapon/rampage-skills rampage-skills)
                  raw-damage
                  (assoc :weapon/raw-damage raw-damage)
                  defense-bonus
                  (assoc :weapon/defense-bonus defense-bonus)
                  affinity
                  (assoc :weapon/affinity affinity)
                  element
                  (assoc :weapon/element element))
                #_{:sharpness sharpness
                   :horn-notes horn-notes
                   :shelling-type shelling-type
                   :phial-type phial-type
                   :kinsect-lv  kinsect-lv
                   :charge-lv-atk charge-lv-atk
                   :coatings coatings
                   :drr drr})))))

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

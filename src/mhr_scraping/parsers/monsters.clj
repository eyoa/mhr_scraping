(ns mhr-scraping.parsers.monsters
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
         string/join
         string/trim)))

(defn drops
  [table]
  (let [header (->> table
                    (hs/select (hs/descendant (hs/tag :th)))
                    (map (fn [entry]
                           (->>
                            (:content entry)
                            (filter string?)
                            (string/join)
                            (string/trim)))))
        body (->> table
                  (hs/select (hs/descendant (hs/tag :tbody)
                                            (hs/tag :tr)))
                  (map (fn [trow]
                         (->> trow
                              (hs/select (hs/descendant
                                          (hs/tag :td)))
                              (map text-content)))))]
    (map (fn [row]
           (apply hash-map (interleave header row)))
         body)))

(defn monster
  [url]
  (->> (:body (http/get url))
       h/parse
       h/as-hickory
       (hs/select (hs/descendant
                   (hs/and (hs/tag :div)
                           (hs/attr :x-show
                                    (fn [x]
                                      (= x "tab === 'items'"))))
                   (hs/descendant (hs/tag :table))))
       first
       drops))

(defn monsters
  [body]
  (->> body
       h/parse
       h/as-hickory
       (hs/select (hs/descendant (hs/tag :aside)
                                 (hs/tag :li)
                                 (hs/tag :a)))
       (map (fn [{:keys [attrs] :as element}]
              (let [href (get attrs :href)
                    name
                    (-> (hs/select (hs/tag :h3) element)
                        first
                        :content
                        string/join)
                    img
                    (-> (hs/select (hs/tag :img) element)
                        first
                        (get-in [:attrs :src]))
                    details
                    (monster href)]
                {:monster/name name
                 :monster/img img
                 :monster/href href
                 :monster/details details})))))

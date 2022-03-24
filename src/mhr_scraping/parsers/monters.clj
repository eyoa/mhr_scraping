(ns mhr-scraping.parsers.monters
  (:require
   [clojure.string :as string]
   [hickory.select :as hickory.select]
   [hickory.core :as hickory]
   [clj-http.client :as client]))


(defn drops
  [table]
  #_table
  (->> table
       (hickory.select/select
        (hickory.select/descendant (hickory.select/tag :th)))
       (map (fn [entry]
              (->>
               (:content entry)
               (filter string?)
               (string/join)
               (string/trim)))))
  (->> table
       (hickory.select/select
        (hickory.select/descendant (hickory.select/tag :tbody)
                                   (hickory.select/tag :tr)))
       (map (fn [trow]
              (->> trow 
                   (hickory.select/select
                    (hickory.select/descendant (hickory.select/tag :td)))
                   (map (fn [entry]

                          (->> entry
                               :content
                               (filter (fn [el]
                                         (= (:tag el) :a)))
                               first
                               :content
                               (filter string?)
                               (string/join)
                               (string/trim))
                          #_(->> entry
                               :content
                               (filter string?)
                               (string/join)
                               (string/trim)))))))))





(defn monster
  [url]
  (->> (:body (client/get "https://mhrise.kiranico.com/data/monsters/366824395"))
      hickory/parse
      hickory/as-hickory
      (hickory.select/select
       (hickory.select/descendant 
        (hickory.select/and (hickory.select/tag :div)
                            (hickory.select/attr :x-show
                                                 (fn [x]
                                                   (= x "tab === 'items'"))))
        (hickory.select/descendant (hickory.select/tag :table))))
      first
      drops))

(defn monsters
  [body]
  (->> body
       hickory/parse
       hickory/as-hickory
       (hickory.select/select
        (hickory.select/descendant (hickory.select/tag :aside)
                                   (hickory.select/tag :li)
                                   (hickory.select/tag :a)))
       (map (fn [{:keys [attrs] :as element}]
              (let [href (get attrs :href)
                    name
                    (-> (hickory.select/select (hickory.select/tag :h3) element)
                        first
                        :content
                        string/join)
                    img
                    (-> (hickory.select/select (hickory.select/tag :img) element)
                        first
                        (get-in [:attrs :src]))
                    details
                    (monster href)]

                {:monster/name name
                 :monster/img img
                 :monster/href href
                 :monster/details details})))))



(defn main
  []
  (monsters
   (:body (client/get "https://mhrise.kiranico.com/data/monsters"))))

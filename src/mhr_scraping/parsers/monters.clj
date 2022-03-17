(ns mhr-scraping.parsers.monters
  (:require
   [clojure.string :as string]
   [hickory.select :as hickory.select]
   [hickory.core :as hickory]))

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
                        (get-in [:attrs :src]))]
                {:monster/name name
                 :monster/img img
                 :monster/href href})))))

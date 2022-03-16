(ns mhr-scraping.core
  (:require [clj-http.client :as client]
            [hickory.select :as hs]
            [hickory.core :as hc]))


(defn -main
  []
  (let [response (client/get "https://mhrise.kiranico.com/data/monsters")]
    (when (= (:status response) 200)
      (->> (:body response) 
           hc/parse 
           hc/as-hickory
           (hs/select (hs/descendant (hs/tag :aside)
                                     (hs/tag :li)
                                     (hs/tag :a)))
           (map (fn [{:keys [attrs] :as element}]
                  (let [href (get attrs :href)
                        name 
                        (-> (hs/select (hs/tag :h3) element)
                            (first)
                            (get :content))
                        
                        img
                        (-> (hs/select (hs/tag :img) element)
                            (first)
                            (get-in [:attrs :src]))]
                    {:name name :img img :href href})
                  ))))))

(ns mhr-scraping.core
  (:require [clj-http.client :as client]
            [hickory.select :as hs]
            [hickory.core :as hc]))

(defn -main
  []
  (let [response (client/get "https://mhrise.kiranico.com/data/monsters")]
    (when (= (:status response) 200)
      (:body response)
      (def parsed-html (-> (:body response) hc/parse hc/as-hickory))
      (-> (hs/select 
           (hs/child (hs/tag :aside)
                     (hs/descendant (hs/tag :ul)))  
           parsed-html)))))

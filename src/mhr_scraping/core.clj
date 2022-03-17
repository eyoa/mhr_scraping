(ns mhr-scraping.core
  (:require [clj-http.client :as client]
            [hickory.select :as hs]
            [hickory.core :as hc]
            [clojure.string :as s]))



(def monsters_res
  (client/get "https://mhrise.kiranico.com/data/monsters"))

(def base_res
(client/get "https://mhrise.kiranico.com/"))

(defn monsters
  [res]
  ; res from /data/monsters
  (->> (:body res) 
           hc/parse 
           hc/as-hickory
           (hs/select (hs/descendant (hs/tag :aside)
                                     (hs/tag :li)
                                     (hs/tag :a)))
           (map (fn [{:keys [attrs] :as element}]
                  (let [href (get attrs :href)
                        name 
                        (-> (hs/select (hs/tag :h3) element)
                            first
                            :content)
                        
                        img
                        (-> (hs/select (hs/tag :img) element)
                            first
                            (get-in [:attrs :src]))]
                    {:monster/name name :monster/img img :monster/href href})
                  ))))

(defn nav
  [res]
  ; response from base page
  (->> (:body res)
           hc/parse
           hc/as-hickory
           (hs/select (hs/descendant (hs/attr :aria-label #(= % "Sidebar"))
                                     (hs/tag :a)))
           (map (fn [element]
                  (let [link_name
                        (s/trim (str ""
                                     (-> (hs/select (hs/tag :a) element)
                                         first
                                         :content
                                         first)))
                        nav_href
                        (-> (hs/select (hs/tag :a) element)
                            first
                            (get-in [:attrs :href]))]
                    {:nav/name link_name :nav/href nav_href}
                    )))))


(defn -main
  []
  (let [response base_res]
    (when (= (:status response) 200)
      #_(monsters response)
      (let [nl (nav response)]
        (->(filter #(= (:nav/name %) "Long Sword") nl)
           first
           :href)
        ))))

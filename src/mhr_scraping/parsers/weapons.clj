(ns mhr-scraping.parsers.weapons
  (:require
   [clojure.string :as string]
   [hickory.select :as hickory.select]
   [hickory.core :as hickory]
   [clj-http.client :as client]))


(defn text-content
  [element]
  (-> element 
      :content
      first
      :content
      string/join
      string/trim))

(defn weapon
  [weapon-type trow]
  (let [name
        (-> (hickory.select/select
             (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                            (hickory.select/nth-child 2))
                                        (hickory.select/tag :div)) trow)
            first
            (text-content))

        rampage-skills
        (->> trow
         (hickory.select/select
          (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                         (hickory.select/nth-child 3))
                                        (hickory.select/tag :div)))
            (map (fn [el]
              (text-content el))))

        raw-dmg
        (-> (hickory.select/select
             (hickory.select/descendant (hickory.select/and  (hickory.select/tag :td)
                                                             (hickory.select/nth-child 4))
                                        (hickory.select/tag :div)) trow)
            first
            :content
            string/join
            string/trim)
        ]
    {:name name
     :raw-dmg raw-dmg
     :rampage-skills rampage-skills}))


(defn weapons
  [body]
  (->> (:body (client/get "https://mhrise.kiranico.com/data/weapons?scope=wp&value=0"))
       hickory/parse
       hickory/as-hickory
       (hickory.select/select
        (hickory.select/descendant (hickory.select/tag :aside)))
       (map (fn [element]
               (let [weapon-type
                       (-> (hickory.select/select
                            (hickory.select/descendant (hickory.select/tag :h3)) element)
                           first
                           :content
                           (string/join)
                           (string/trim))
                     weapon-list
                     (->> element 
                          (hickory.select/select
                           (hickory.select/descendant (hickory.select/tag :tbody)
                                                      (hickory.select/tag :tr)))
                          (map weapon weapon-type))]
                 
                   {:weapon/weapon-type weapon-type
                    :weapon/weapon-list weapon-list})))))

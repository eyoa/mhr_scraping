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

(defn get-element
  [element]
  (if (empty? element)
    nil
    (map (fn [el]
           (if (= (:type el) :element)
             (-> el
                 :content
                 first
                 (get-in [:attrs :src])
                 (string/split #"/")
                 last
                 (string/split #"\.")
                 first)
             (string/trim el))) element )))

(defn weapon
  [weapon-type tbody]
  (->> tbody 
       first
   (hickory.select/select
    (hickory.select/descendant (hickory.select/tag :tr)))
   (map (fn [trow]
          (let [name
                (-> (hickory.select/select
                     (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                    (hickory.select/nth-child 2))
                                                (hickory.select/tag :div)) trow)
                    first
                    (text-content))
                
                img
                (-> (hickory.select/select
                     (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                    (hickory.select/nth-child 1))
                                                (hickory.select/tag :img)) trow)
                    first
                    (get-in [:attrs :src]))

                decos
                (->> trow 
                     (hickory.select/select
                      (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                     (hickory.select/nth-child 2))
                                                 (hickory.select/tag :img)))
                     (seq)
                     (map (fn [el]
                            (-> (get-in el [:attrs :src]) 
                                (string/split #"/")
                                last
                                (string/split #"\.")
                                first
                                ))))

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

                element
                (-> (hickory.select/select
                     (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                    (hickory.select/nth-child 5))
                                                (hickory.select/tag :span)) trow)
                    ;;some weapons have 2 elements re-visit later
                    first
                    :content
                    (get-element))
                
                affinity
                (-> (hickory.select/select
                     (hickory.select/child (hickory.select/and (hickory.select/tag :td)
                                                               (hickory.select/nth-child 5))
                                           (hickory.select/tag :div)
                                           (hickory.select/tag :span)) trow)
                    last
                    :content
                    first
                    (string/join)
                    (string/trim))
                
                sharpness
                (if (some #{weapon-type} '("Bow" "Light Bowgun" "Heavy Bowgun"))
                  (str "ranged weapon")  
                  (->> trow 
                       (hickory.select/select
                         (hickory.select/child (hickory.select/and (hickory.select/tag :td)
                                                                   (hickory.select/nth-child 6))
                                               (hickory.select/tag :div)))
                      first
                      :content
                      first
                      :content
                      (map (fn [entry]
                             (get-in entry [:attrs :style])))
                      
                                         ))
                
                ]
            {:name name
             :img img
             :decos decos
             :raw-dmg raw-dmg
             :rampage-skills rampage-skills
             :element element
             :affinity affinity
             :sharpness sharpness})))))


(defn weapons
  [body]
  (->> (:body (client/get "https://mhrise.kiranico.com/data/weapons?scope=wp&value=0"))
       #_body
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
                           (hickory.select/descendant (hickory.select/tag :tbody)))
                          (weapon weapon-type))]
                 
                   {:weapon/weapon-type weapon-type
                    :weapon/weapon-list weapon-list})))))


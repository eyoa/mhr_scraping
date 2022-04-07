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

(defn get-px-amt
  [value]
  (Integer. (re-find #"\d+" value )))


(defn get-element
  [element]
  (if (empty? element)
    nil
    (->> element
         (map (fn [{multi-el :content}]
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
                         (string/trim el))) multi-el ))))))


(defn get-sharpness
  [elements]
  (let [base
        (->> elements
            first
            :content
            (map (fn [entry]
                   (-> (get-in entry [:attrs :style])
                       (string/split #";|:")
                       second
                       (get-px-amt)
                       float
                       (* 100 (/ 88))))))
        
        bonus
        (->> elements
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
                     (hickory.select/child (hickory.select/and (hickory.select/tag :td)
                                                                    (hickory.select/nth-child 5))
                                                (hickory.select/tag :span)) trow)
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
               
                
                def-bonus
                (-> (hickory.select/select
                     (hickory.select/child (hickory.select/and (hickory.select/tag :td)
                                                               (hickory.select/nth-child 5))
                                           (hickory.select/and (hickory.select/tag :div)
                                                               (hickory.select/find-in-text #"Defense"))) trow)
                    first
                    :content
                    (some->
                     first
                     (string/split #" ")
                     last))

                sharpness
                (if (some #{weapon-type} '("Bow" "Light Bowgun" "Heavy Bowgun"))
                  nil  
                  (->> trow 
                       (hickory.select/select
                         (hickory.select/child (hickory.select/and (hickory.select/tag :td)
                                                                   (hickory.select/nth-child 6))
                                               (hickory.select/tag :div)))
                      first
                      :content
                      (get-sharpness)))

                horn-notes
                (if (= weapon-type "Hunting Horn")
                  (->> trow
                     (hickory.select/select
                      (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                     (hickory.select/nth-child 7))
                                                 (hickory.select/tag :div)))
                     (map (fn [el]
                            (-> el
                                :content
                                first
                                (string/trim))))))

                shelling-type
                (if (= weapon-type "Gunlance")
                  (->> trow
                     (hickory.select/select
                      (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                     (hickory.select/nth-child 7))
                                                 (hickory.select/tag :div)))
                     (map (fn [el]
                            (-> el
                                :content
                                first
                                (string/trim))))))

                phial-type
                ;;some of these have values?? check if they can be left out
                (if (some #{weapon-type} '("Switch Axe" "Charge Blade"))
                  (->> trow
                     (hickory.select/select
                      (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                     (hickory.select/nth-child 7))
                                                 (hickory.select/tag :div)))
                     (map (fn [el]
                            (-> el
                                :content
                                first
                                (string/trim))))))

                kinsect-lv
                (if (= weapon-type "Insect Glaive")
                  (->> trow
                     (hickory.select/select
                      (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                     (hickory.select/nth-child 7))
                                                 (hickory.select/tag :div)))
                     (map (fn [el]
                            (-> el
                                :content
                                first
                                (string/trim))))))
                
                charge-lv-atk
                ;; last item is greyed out... is green or grey class required info?
                (if (= weapon-type "Bow")
                  (->> trow
                     (hickory.select/select
                      (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                     (hickory.select/nth-child 6))
                                                 (hickory.select/tag :div)))
                     (map (fn [el]
                            (-> el
                                :content
                                first)))))

                coatings
                ;; check for grey in class?? is green or grey class required info?
                (if (= weapon-type "Bow")
                  (->> trow
                     (hickory.select/select
                      (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                     (hickory.select/nth-child 7))
                                                 (hickory.select/tag :div)))
                     (map (fn [el]
                            (-> el
                                :content
                                first
                                (string/trim))))))

                #_drr
                #_(if (some #{weapon-type} '("Heavy Bowgun" "Light Bowgun"))
                  (->> trow
                     (hickory.select/select
                      (hickory.select/descendant (hickory.select/and (hickory.select/tag :td)
                                                                     (hickory.select/nth-child 6))
                                                 (hickory.select/tag :div)))
                     (map (fn [el]
                            (-> el
                                :content
                                first
                                (string/trim)
                                (string/split #" " 2)
                                )))))

                ]
            {:name name
             :img img
             :decos decos
             :raw-dmg raw-dmg
             :rampage-skills rampage-skills
             :element element
             :affinity affinity
             :def-bonus def-bonus
             :sharpness sharpness
             :horn-notes horn-notes
             :shelling-type shelling-type
             :phial-type phial-type
             :kinsect-lv  kinsect-lv
             :charge-lv-atk charge-lv-atk
             :coatings coatings
             #_:drr #_drr})))))


(defn weapons
  [body]
  (->> (:body (client/get "https://mhrise.kiranico.com/data/weapons?scope=wp&value=10"))
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

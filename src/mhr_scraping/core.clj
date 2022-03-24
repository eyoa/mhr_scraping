(ns mhr-scraping.core
  (:require
   [clojure.string :as string]
   [clj-http.client :as client]
   [hickory.select :as hickory.select]
   [hickory.core :as hickory]
   [mhr-scraping.parsers.monters :as parsers.monsters]))

(defmulti parse
  (fn [nav]
    (or (:nav/parent nav)
        (:nav/name nav))))

(defmethod parse :default
  [nav]
  nil)

(defmethod parse "Monsters"
  [nav]
  (parsers.monsters/monsters (:body (client/get (:nav/href nav)))) 
)

(def domain
  "https://mhrise.kiranico.com/")

(defn parse-subnav
  [[button subnav]]
  (map (fn [el]
         {:nav/parent (->> (:content button)
                           (filter string?)
                           string/join
                           string/trim)
          :nav/name (->> (:content el)
                         (filter string?)
                         string/join
                         string/trim)
          :nav/href (get-in el [:attrs :href])})
       (hickory.select/select (hickory.select/child (hickory.select/tag :a))
                              subnav)))

(defn nav
  []
  (->> (client/get domain)
       :body
       hickory/parse
       hickory/as-hickory
       (hickory.select/select
        (hickory.select/child
         (hickory.select/and (hickory.select/tag :nav)
                             (hickory.select/attr :aria-label
                                                  (fn [s]
                                                    (= s "Sidebar"))))))
       first
       :content
       first
       :content
       (filter (fn [el]
                 (= (:type el) :element)))
       (partition-by (fn [el]
                       (= (:tag el) :a)))
       (mapcat (fn [elements]
                 (if (= (:tag (first elements)) :a)
                   (map (fn [el]
                          {:nav/name (->> (:content el)
                                          (filter string?)
                                          string/join
                                          string/trim)
                           :nav/href (get-in el [:attrs :href])})
                        elements)
                   (->> (first elements)
                        :content
                        (filter (fn [el] (= (:type el) :element)))
                        parse-subnav))))))

(defn -main
  []
  (mapcat parse (nav))
)


(ns mhr-scraping.core
  (:require
   [clojure.string :as string]
   [hickory.core :as h]
   [hickory.select :as hs]
   [mhr-scraping.http :as http]
   [mhr-scraping.parsers.monsters :as parsers.monsters]
   [mhr-scraping.parsers.weapons :as parsers.weapons]))

(defmulti parse
  (fn [nav]
    (or (:nav/parent nav)
        (:nav/name nav))))

(defmethod parse :default
  [nav]
  nil)

#_(defmethod parse "Monsters"
    [nav]
    (parsers.monsters/monsters (:body (http/get (:nav/href nav)))))

(defmethod parse "Weapons"
  [nav]
  (parsers.weapons/weapons (:body (http/get (:nav/href nav)))))

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
       (hs/select (hs/child (hs/tag :a))
                  subnav)))

(defn nav
  []
  (->> (http/get domain)
       :body
       h/parse
       h/as-hickory
       (hs/select (hs/child (hs/and (hs/tag :nav)
                                    (hs/attr :aria-label
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
  (->> (nav)
       (pmap parse)
       doall
       (apply concat)))

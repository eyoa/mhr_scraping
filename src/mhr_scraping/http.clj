(ns mhr-scraping.http
  "Cache HTTP responses for faster development experience"
  (:refer-clojure :exclude [get])
  (:require
   [clj-http.client :as client]))

(def get
  (memoize client/get))

(defn exists?*
  [url]
  (= (:status (client/head url
                           {:throw-exceptions? false}))
     200))

(def exists?
  (memoize exists?*))

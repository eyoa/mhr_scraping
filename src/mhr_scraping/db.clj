(ns mhr-scraping.db
  (:require
   [clojure.java.io :as io]
   [xtdb.api :as xt]))

(defn start-xtdb!
  []
  (letfn [(kv-store [dir]
            {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                        :db-dir (io/file dir)
                        :sync? true}})]
    (xt/start-node
     {:xtdb/tx-log (kv-store "data/dev/tx-log")
      :xtdb/document-store (kv-store "data/dev/doc-store")
      :xtdb/index-store (kv-store "data/dev/index-store")})))

(defonce xtdb-node (start-xtdb!))

(defn stop-xtdb!
  []
  (.close xtdb-node))


(comment
  (xt/submit-tx xtdb-node
                [[::xt/put
                  {:xt/id "hi2u1"
                   :user/name "Evelyn"}]
                 [::xt/put
                  {:xt/id "hi2u2"
                   :user/name "Brendon"
                   :user/screenname "niamu"}]])

  (xt/q (xt/db xtdb-node #inst "2022-03-24T19:44:29.817-00:00")
        '{:find [(pull ?u [*])]
          :where [[?u :user/name ?n]
                  #_[?u :user/screenname _]]})

  (xt/submit-tx xtdb-node
                [[::xt/evict "hi2u2"]])

  (xt/entity-history (xt/db xtdb-node) "hi2u2" :desc {:with-docs? true})

  (clojure.set/intersection #{1 2 3}
                            (set [3 4 5])))

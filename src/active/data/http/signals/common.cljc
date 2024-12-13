(ns ^:no-doc active.data.http.signals.common
  (:require [active.data.record :as r #?@(:cljs [:include-macros true])]))

(r/def-record Context
  [server-backend
   client-backend])

(r/def-record ServerBackend
  [broadcast-fn])

(r/def-record ClientBackend []
  [usage-counter ;; atom int
   last-signal ;; atom [timestamp signal]
   connected-state ;; atom :connecting :open :closed
   ])

;; Note: connection-idle-timeout-ms just reflects what is currently fixed in ring. Cannot change it.
;; https://github.com/ring-clojure/ring/issues/513
(def connection-idle-timeout-ms (* 30 1000))
(def keep-alive-interval-ms (/ connection-idle-timeout-ms 2))

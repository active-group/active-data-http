(ns active.data.http.rpc.common
  (:require [active.data.record :as r #?@(:cljs [:include-macros true])]
            [active.data.realm :as realm]))

(r/def-record context
  [context-format ;; format/format
   context-underlying-format :- (realm/enum :transit)
   context-path :- realm/string
   context-create-caller ;; rpc -> fn
   ])

(r/def-record rpc
  [rpc-name
   rpc-context
   rpc-result-realm
   rpc-parameter-realms])

(defn create-caller [rpc]
  (if-let [c (context-create-caller (rpc-context rpc))]
    (c rpc)
    (fn [& _args]
      (throw (ex-info "RPC cannot be called as a function. Define a calling conventin in the context." {})))))

(defn rpc-path [rpc]
  (str (rpc-name rpc)))

(defn rpc-full-path [rpc]
  (if-let [cp (not-empty (context-path (rpc-context rpc)))]
    (str cp "/" (rpc-path rpc))
    (rpc-path rpc)))

(defn request-realm [rpc]
  (realm/map-with-keys {:args (apply realm/tuple (rpc-parameter-realms rpc))}))

(defn response-realm [rpc]
  (realm/map-with-keys {:result (rpc-result-realm rpc)}))

(defn response [result]
  {:result result})

(def response-result :result)

(defn request [args]
  {:args (vec args)})

(def request-args :args)

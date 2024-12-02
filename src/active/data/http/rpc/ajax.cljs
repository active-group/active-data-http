(ns ^:no-doc active.data.http.rpc.ajax
  (:require [active.data.http.rpc.common :as common]
            [active.data.http.ajax :as ajax]))

(defn ^:no-doc prepare-request [rpc]
  (let [context (common/rpc-context rpc)]
    (assert (= :transit (common/context-underlying-format context)))
    (-> {:method "POST"
         :uri (common/rpc-full-path rpc)
         :realm (common/request-realm rpc)
         :response-realm (common/response-realm rpc)}
        (ajax/prepare-use-transit-format (common/context-format context)))))

;; Note: this could be something that returns a promise of the result of an ajax call.
#_(defn caller [rpc]
    (let [prep (prepare-request rpc)]
      (fn [handler & args] ;; or take a map? error-handler etc.
        (-> prep
            (assoc :handler (comp handler common/response-result))
            (assoc :params (common/request args))))))

#_(defn call [rpc handler & args]
    (apply (caller rpc) handler args))


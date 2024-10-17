(ns active.data.http.rpc.reacl-c
  (:require [active.data.http.rpc.common :as common]
            [active.data.http.ajax :as ajax]
            [active.data.http.rpc.ajax :as rpc.ajax]
            [reacl-c-basics.ajax :as reacl-c-ajax]))

(defn caller [rpc]
  (let [prep (rpc.ajax/prepare-request rpc)]
    (fn [& args]
      (let [options (-> prep
                        (dissoc :response-realm)
                        (assoc :params (common/request args)))
            uri (common/rpc-full-path rpc)
            wrapper (ajax/response-wrapper (common/context-format (common/rpc-context rpc))
                                           (:response-realm prep))]
        (-> (reacl-c-ajax/POST uri options)
            (reacl-c-ajax/map-ok-response (wrapper common/response-result)))))))
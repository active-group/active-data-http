(ns active.data.http.rpc.reitit
  (:require [active.data.http.rpc :as rpc]
            [active.data.http.rpc.common :as common]
            [reitit.ring :as ring]
            [reitit.ring.coercion :as rrc]
            [active.data.http.reitit :as reitit]))

(defn impl-ext [rpc-var spec]
  (let [rpc (rpc/resolve-rpc rpc-var)]
    [(str "/" (common/rpc-path rpc)) {:post (-> spec
                                                (update :handler (fn [handler]
                                                                   (when handler
                                                                     (fn [request]
                                                                       (let [response (handler (assoc request :args (common/request-args (:body (:parameters request)))))]
                                                                         (cond-> response
                                                                           (= 200 (:status response))
                                                                           (update :body common/response)))))))
                                                (update :parameters assoc :body (common/request-realm rpc))
                                                (update :responses (fn [m]
                                                                     (assoc (or m {})
                                                                            200
                                                                            {:body (common/response-realm rpc)}))))}]))

(defn impl [rpc-var handler]
  (impl-ext rpc-var {:handler (fn [request]
                                {:status 200
                                 :body (apply handler (:args request))})}))

(defn context-router [context implementations]
  ;; TODO: assert all implemented rpcs are from this context?
  ;; TODO: need to set/fix transit?
  (assert (= :transit (common/context-underlying-format context)))
  (ring/router
   [(common/context-path context) (vec (apply concat implementations))]
   {:data {:coercion (reitit/realm-coercion (common/context-format context))
           :middleware [rrc/coerce-exceptions-middleware
                        rrc/coerce-request-middleware
                        rrc/coerce-response-middleware]}
    ;; TODO: allow user to add options/middleware.
    }))

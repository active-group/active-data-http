(ns active.data.http.rpc.reitit
  (:require [active.data.http.rpc :as rpc]
            [active.data.http.rpc.common :as common]
            [reitit.ring :as ring]
            [reitit.ring.coercion :as rrc]
            [active.data.http.reitit :as reitit]))

(defn impl-ext [rpc-var spec]
  (let [rpc (rpc/resolve-rpc rpc-var)]
    [(str "/" (common/rpc-path rpc))
     {:post (-> spec
                (update :handler (fn [handler]
                                   (when handler
                                     (fn [request]
                                       (assert (some? (:body (:parameters request)))
                                               "Missing body parameters in request. Did you add reitit.ring.middleware.parameters/parameters-middlware to the middware chain?")
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

(defn context-router
  "Returns a [[reitit.ring/router]] defining routes that implement the
   given rpcs. Use [[impl]] or [[impl-ext]] to specify the
   implementations. The `opts` parameter is the same as for [[reitit.ring/router]].

  Important note: To use this, you need a transit parser middleware in
  your middleware chain, like muuntaja for example
  [https://github.com/metosin/reitit/blob/master/doc/ring/content_negotiation.md].
  
  You can add this higher up, or via the `opts` parameter like
  ```
  {:data {:muuntaja muuntaja.core/instance
          :middleware [reitit.ring.middleware.muuntaja/format-middleware]}}
  ```

  You will also need to add
  [[reitit.ring.middleware.parameters/parameters-middleware]] to you
  middlware chain. If you don't need it anywhere else in your
  application, set is via the `opts` too:

  ```
  {:data {:middleware [reitit.ring.middleware.parameters/parameters-middlware]}}
  ```"
  ([context implementations]
   (context-router context implementations nil))
  ([context implementations opts]
   ;; TODO: assert all implemented rpcs are from this context?
   ;; TODO: need to set/fix transit?
   (assert (= :transit (common/context-underlying-format context)))
   (ring/router
    [(common/context-path context) (vec (apply concat implementations))]
    (merge opts
           {:data (merge (:data opts)
                         {:coercion (reitit/realm-coercion (common/context-format context))
                          :middleware (vec (concat (:middleware (:data opts))
                                                   [;; rrc/coerce-exceptions-middleware ;; <- TODO: what's exceptions-middleware for? need it? try without for now.
                                                    rrc/coerce-request-middleware
                                                    rrc/coerce-response-middleware]))})}))))

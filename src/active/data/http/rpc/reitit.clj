(ns active.data.http.rpc.reitit
  (:require [active.data.http.rpc :as rpc]
            [active.data.http.rpc.common :as common]
            [reitit.ring.coercion :as rrc]
            [active.data.http.reitit :as reitit]))

(defn impl-ext
  "Specifies an rpc implemtation as a ring handler spec, where the
  `:handler` takes a request map and returning a response map. The
  request map will contain an `:args` key, containing the list of
  arguments of the rpc call, and must return the rpc result as the
  `:body` in the response map.

  ```
  (impl-ext #'my-api/plus
            {:handler (fn [req]
                        {:status 200 :body (apply + (:args req))})})
  ```
  "
  [rpc-var spec]
  (let [rpc (rpc/resolve-rpc rpc-var)]
    [(str "/" (common/rpc-path rpc))
     {:post (-> spec
                (update :handler (fn [handler]
                                   (when handler
                                     (fn [request]
                                       (assert (some? (:body (:parameters request)))
                                               "Missing body parameters in request. Adding reitit.ring.middleware.parameters/parameters-middlware to the middware chain may fix this.")
                                       (let [response (handler (assoc request :args (common/request-args (:body (:parameters request)))))]
                                         (cond-> response
                                           (= 200 (:status response))
                                           (update :body common/response)))))))
                (update :parameters assoc :body (common/request-realm rpc))
                (update :responses (fn [m]
                                     (assoc (or m {})
                                            200
                                            {:body (common/response-realm rpc)}))))}]))

(defn impl
  "Specifies an rpc implemtation as a function with the arguments and
return value exaclty as declared in the rpc declaration via
  [[active.data.http.rpc/defn-rpc]].

  ```
  (impl #'my-api/plus +)
  ```
  "
  [rpc-var handler]
  (impl-ext rpc-var {:handler (fn [request]
                                {:status 200
                                 :body (apply handler (:args request))})}))

(defn- context-routes* [context implementations]
  ;; TODO: assert all implemented rpcs are from this context?
  ;; TODO: need to set/fix transit?
  (assert (= :transit (common/context-underlying-format context)))
  [(common/context-path context) (vec implementations)])

(defn context-routes
  "Returns a reitit routes vector defining routes that implement the
   given rpcs. Use [[impl]] or [[impl-ext]] to specify the
   implementations. The `opts` parameter is the same as for [[reitit.ring/router]].

  ```
  (context-routes my-api/context [(impl #'my-api/plus +)])
  ```

  Important note: To use this, you need a transit parser middleware in
  your middleware chain, like muuntaja for example
  [https://github.com/metosin/reitit/blob/master/doc/ring/content_negotiation.md].
  
  You can add this higher up, for example like this:
  ```
  [\"\" {:muuntaja muuntaja.core/instance
         :middleware [reitit.ring.middleware.muuntaja/format-middleware]}
   (context-routes ...)]
  ```

  You will also need to add
  [[reitit.ring.middleware.parameters/parameters-middleware]] to you
  middlware chain. If you don't need it anywhere else in your
  application anyway, add it like this:

  ```
  [\"\" {:middleware [reitit.ring.middleware.parameters/parameters-middlware]}
   (context-routes ...)]
  ```"
  [context implementations]
  ["" {:coercion (reitit/realm-coercion (common/context-format context))
       ;; Note: coerce-exceptions: "turns coercion exceptions into pretty responses"
       ;; ...not ideal, but better than empty responses, which we get otherwise.
       :middleware [rrc/coerce-exceptions-middleware
                    rrc/coerce-request-middleware
                    rrc/coerce-response-middleware]}
   (context-routes* context implementations)])

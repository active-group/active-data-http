(ns active.data.http.signals.reitit
  (:require [active.data.http.signals.common :as c]
            [active.data.http.signals.ring :as ring]))

(defn context-routes [context]
  (let [backend (c/server-backend context)
        endpoint {:handler (ring/upgrade-handler backend)}]
    [(ring/route backend) {:get endpoint
                           :post endpoint}]))


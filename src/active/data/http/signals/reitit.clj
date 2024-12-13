(ns active.data.http.signals.reitit
  (:require [active.data.http.signals.common :as c]
            [active.data.http.signals.ring :as ring]
            [reitit.ring :as reitit-ring]))

(defn router [context & [opts]]
  (let [backend (c/server-backend context)]
    (let [endpoint {:handler (ring/upgrade-handler backend)}]
      (reitit-ring/router
       [(ring/route backend) {:get endpoint
                              :post endpoint}]
       opts))))

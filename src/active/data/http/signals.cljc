(ns active.data.http.signals
  (:require [active.data.http.formats.transit :as transit]
            [active.data.http.signals.common :as c]
            #?(:clj [active.data.http.signals.ring :as ring])
            #?(:cljs [active.data.http.signals.js-websocket :as js-ws])))

(defn context
  "Creates a signal context by defining the relative URI of the endpoint to use, and the realm of the signals."
  [uri signals-realm & {format :format server-backend :server-backend client-backend :client-backend}]
  (let [format (or format transit/transit-format)]
    (c/Context c/server-backend #?(:clj (or server-backend (ring/create-backend uri signals-realm format))
                                   :cljs server-backend)
               c/client-backend #?(:clj client-backend
                                   :cljs (or client-backend (js-ws/create-backend uri signals-realm format))))))

#?(:clj
   (defn broadcast!
     "Sends the given signal to all currently connected receivers."
     [context signal]
     (let [backend (c/server-backend context)]
       ((c/broadcast-fn backend) signal))))

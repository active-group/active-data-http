(ns ^:no-doc active.data.http.signals.ring
  (:require [active.data.record :as r]
            [active.data.http.signals.common :as c]
            [active.data.translate.core :as translate]
            [cognitect.transit :as transit]
            [ring.websocket            :as ws]
            [ring.websocket.protocols  :as ws.protocols])
  (:import [java.io ByteArrayOutputStream]))

(r/def-record RingBackend
  :extends c/ServerBackend
  [connections
   route])

(defn- to-transit-str [v]
  (let [s (ByteArrayOutputStream.)
        w (transit/writer s :json)]
    (transit/write w v)
    (.toString s "UTF-8")))

(defn create-backend [uri signals-realm format]
  (let [to-string (comp to-transit-str
                        (translate/translator-from signals-realm format))
        conns (atom #{})]
    (RingBackend c/broadcast-fn (fn [signal]
                                  ;; translate to str or binary.
                                  ;; send to all connections.
                                  (let [msg (to-string signal)]
                                    (doseq [sock @conns]
                                      ;; Note: send has succeed and fail callbacks
                                      (ws/send sock msg))))
                 route uri
                 connections conns)))

(defn- listener [backend]
  (reify ws.protocols/Listener
    (on-open    [_ socket]
      (println (pr-str socket))
      (swap! (connections backend) conj socket))
    (on-close   [_ socket _status _reason]
      (swap! (connections backend) disj socket))
    (on-message [_ _socket _msg]
      ;; we'll get msg="keep-alive" from the client here, which keeps the connection alive.
      nil)
    (on-error   [_ _socket _error]
      #_(instance? ClosedChannelException error)
      nil)
    (on-pong    [_ _socket _data]
      nil)))

(defn upgrade-handler [backend]
  (assert (r/is-a? RingBackend backend))
  (let [ltsnr (listener backend)]
    (fn [req]
      (if (ws/upgrade-request? req)
        (let [upgrade-req           req
              provided-subprotocols (:websocket-subprotocols upgrade-req)
              _provided-extensions  (:websocket-extensions upgrade-req)]
          {:ring.websocket/protocol (first provided-subprotocols)
           :ring.websocket/listener ltsnr})
        {:status 200 :body "This is a websocket endpoint and can only be accessed through a websocket connection."}))))

(ns ^:no-doc active.data.http.signals.js-websocket
  (:require [active.data.http.signals.common :as c]
            [active.data.translate.core :as translate]
            [active.data.record :as r :include-macros true]
            [cognitect.transit :as transit]))

(r/def-record ^:private WebsocketBackend
  :extends c/ClientBackend
  [])

(defn- create-ws [uri signals-realm format on-open on-close on-message]
  (let [from-string (comp (translate/translator-to signals-realm format)
                          (partial transit/read (transit/reader :json)))]
    (doto (js/WebSocket. uri)
      ;; The error event is fired when a connection with a WebSocket has
      ;; been closed due to an error (some data couldn't be sent for
      ;; example). Or if connect failed; but that also calls onclose
      ;; (even on first connect)
      (aset "onerror" (fn [_ev]
                        nil))

      (aset "onmessage" (fn [ev]
                          ;; ev.origin
                          (let [msg (.-data ev)]
                            (cond
                              (string? msg)
                              ;; TODO: some callback for formatting errors?
                              (let [signal (from-string msg)]
                                (on-message (.-target ev) signal))

                              ;; TODO: else error? (binary types)
                              ))))
      (aset "onclose" (fn [ev]
                        ;; Note: also called on connection error
                        ;; code=1006, wasClean=false

                        ;; ev.code, ev.reason, ev.wasClean
                        (on-close (.-target ev))))
      (aset "onopen" (fn [ev]
                       (on-open (.-target ev))))

      ;; (aset "binaryType" "blob") ;; or "arraybuffer"?
      )))

(defn- stop-pong! [id-atom]
  (when-let [id @id-atom]
    (js/window.clearTimeout id)))

(defn- pong! [^js/WebSocket ws id-atom callback]
  ;; Note: we have to do our own repeated message sending (we don't
  ;; have access to native 'ping&pong' as defined by the websocket
  ;; standard), and at least Chrome doesn't seem to do it on its own.

  ;; Note: check if any previous pong has even been seen by the
  ;; server (a bit of back pressure)
  (when (= 0 (.-bufferedAmount ws))
    (.send ws "keep-alive"))
  ;; stop previous timer, set new one.
  (stop-pong! id-atom)
  (reset! id-atom
          (js/window.setTimeout (partial callback :pong)
                                c/keep-alive-interval-ms)))

(defn- re-connect! [backend uri signals-realm format reconnect-timeout-ms callback]
  (assert (r/is-a? WebsocketBackend backend))
  (create-ws uri signals-realm format
             (fn open [ws]
               (callback :open))
             (fn close [ws]
               ;; delay this a little bit, to not go crazy with reconnect-attempts while server restarts
               (js/window.setTimeout (partial callback :close)
                                     reconnect-timeout-ms))
             (fn message [_ws signal]
               (reset! (c/last-signal backend)
                       [(js/Date.) signal]))))

(defn- lifecycle! [backend current-ws pong-id uri signals-realm format reconnect-timeout-ms event]
  ;; (js/console.log "life cylce event" event)
  (let [keep-alive? (> @(c/usage-counter backend) 0)
        ws @current-ws
        open? (and ws
                   (= js/WebSocket.OPEN (.-readyState ^js/WebSocket ws)))
        closed? (or (not ws)
                    (= js/WebSocket.CLOSED (.-readyState ^js/WebSocket ws)))
        recurse (fn [event]
                  (lifecycle! backend current-ws pong-id uri signals-realm format reconnect-timeout-ms event))]
    (reset! (c/connected-state backend)
            (if open?
              :open
              (if keep-alive?
                :connecting
                :closed)))
    (if keep-alive?
      (if open?
        (pong! ws pong-id recurse)
        (when closed?
          (reset! current-ws
                  (re-connect! backend uri signals-realm format reconnect-timeout-ms recurse))))
      ;; else just stop ponging, server will close ws eventually, or usage-counter increases again.
      (stop-pong! pong-id))))

(defn create-backend [uri signals-realm format]
  (let [backend (WebsocketBackend c/usage-counter (atom 0)
                                  c/connected-state (atom :closed)
                                  c/last-signal (atom nil))
        reconnect-timeout-ms 1000 ;; configurable?
        pong-id (atom nil)
        current-ws (atom nil)
        call! (fn [event]
                (lifecycle! backend current-ws pong-id uri signals-realm format reconnect-timeout-ms event))]
    (add-watch (c/usage-counter backend)
               ::connector
               (fn [_ _ _old-value _new-value]
                 (call! :keep-alive-change)))
    backend))

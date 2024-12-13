(ns active.data.http.signals.reacl-c
  "Reacl-c items for receiving signals."
  (:require [active.data.http.signals.common :as common]
            [active.clojure.lens :as lens]
            [reacl-c.core :as c :include-macros true]))

(c/defn-subscription ^:private atom-sub deliver! [atom init?]
  (let [key (js/Object.)]
    (add-watch atom key
               (fn [_ _ _ new-value]
                 (deliver! new-value)))
    (when init?
      (deliver! @atom))
    (fn []
      (remove-watch atom key))))

(c/defn-item connection-status
  "An item that sets its state to the connection status
  of the given signal context. The connection-status can be :open,
  :closed or :connecting."
  [context]
  ;; Note: does not keep the connection alive, of course. (or make this an option?)
  (-> (atom-sub (common/connected-state (common/client-backend context))
                true)
      (c/handle-action (fn [_ st]
                         st))))

(c/defn-item connected?
  "An item that sets its state to a boolean, indicating if a connection
  is currently in use for the given signal context."
  [context]
  (-> (connection-status context)
      (c/handle-state-change (fn [_ s]
                               (= s :open)))))

(c/defn-subscription keep-alive
  "An item that tries to keep a connection for the given signal context
  open at all times."
  _deliver! [context]
  (let [backend (common/client-backend context)]
    (swap! (common/usage-counter backend) inc)
    (fn []
      (swap! (common/usage-counter backend) dec))))

(defn- raw-receive-signals [context]
  (atom-sub (common/last-signal (common/client-backend context)) false))

(c/defn-item receive-all
  "An item that emits any received signal of the given context as an
  action. Keeps the connection alive while mounted."
  [context]
  (c/fragment (keep-alive context)
              (-> (raw-receive-signals context)
                  (c/handle-action (fn [_ [_timestamp signal]]
                                     (c/return :action signal))))))

(c/defn-subscription ^:private delay-sub deliver! [ms _key action]
  (let [id (js/window.setTimeout #(deliver! action) ms)]
    (fn []
      (js/window.clearTimeout id))))

(defn- only-keep-last-action-within [item ms]
  (if (zero? ms)
    item
    (c/with-state-as [_ last :local nil]
      (c/fragment
       (-> (c/focus lens/first item)
           (c/handle-action (fn [[st _a] a]
                              (c/return :state [st [a]]))))
       ;; Note: using the action as the key, so (only) a different one will restart the delay.
       (when-let [[action] last]
         (delay-sub ms action action))))))

(c/defn-item receive
  "An item that emits a timestamp when the given signal of the given
  context is received. Keeps the connection alive while mounted."
  [context signal & {defer-ms :defer-ms}]
  ;; Note: when multiple identical signals come in in a short period of time, only one is emitted.
  (let [defer-ms (or defer-ms 100)]
    (c/fragment (keep-alive context)
                (-> (raw-receive-signals context)
                    (c/handle-action (fn [_ [timestamp rcv-signal]]
                                       (if (= signal rcv-signal)
                                         (c/return :action timestamp)
                                         (c/return))))
                    ;; Note: because each action is different (timestamp), a new one will restart the delay.
                    (only-keep-last-action-within defer-ms)))))

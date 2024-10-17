(ns active.data.http.rpc
  (:require [active.data.http.rpc.common :as common]
            [active.data.http.builtin :as builtin]))

(defn context [path & {realm-format :format underlying :underlying-format caller :caller}]
  ;; Note: for now we expect the format to support common.request/response (could add that explicitly if needed)
  (common/context {common/context-format (or realm-format builtin/transit-format)
                   common/context-underlying-format (or underlying :transit)
                   common/context-path path
                   ;; e.g. pass active.data.http.rpc.reacl-c/caller as the caller for cljs (default nil to not have the depedency)
                   common/context-create-caller caller}))

(defn set-context-caller [context caller]
  (assoc context common/context-create-caller caller))

(defn ^:no-doc make-rpc [context name result-realm parameter-realms]
  (common/rpc {common/rpc-name name
               common/rpc-context context
               ;; TODO use (realm/enum nil) as default result?
               common/rpc-result-realm result-realm
               common/rpc-parameter-realms parameter-realms}))

(defn ^:no-doc resolve-rpc [var]
  (::rpc (meta var)))

(defn ^:no-doc parse-param-realms
  [params]
  ;; realms are required for now.
  (assert (vector? params) (str "Expected params vector, but got: " (pr-str params)))
  (->> params
       (partition 3)
       (map (fn [[_pname arr prealm]]
              (assert (= arr :-))
              prealm))
       (into [])))

(defn ^:no-doc parse-return-and-params [args]
  (if (= (first args) :-)
    (do (assert (= 3 (count args)))
        [(second args) (parse-param-realms (second (rest args)))])
    (do (assert (empty? (rest args)))
        [nil (parse-param-realms (first args))])))

(defmacro defn-rpc
  "(defn-rpc foo context \"docstring\" :- realm/string [x :- realm/integer])"
  [name context & more]
  ;; TODO: maybe allow metadata that overrides the rpc-path?
  (let [[docstring more] (if (string? (first more))
                           [(first more) (rest more)]
                           [nil more])
        [result-realm param-realms] (parse-return-and-params more)]
    `(let [rpc# (make-rpc ~context '~name ~result-realm ~param-realms)]
       (def ~name (common/create-caller rpc#))
       (alter-meta! (var ~name) assoc ::rpc rpc#)
       (when ~docstring
         (alter-meta! (var ~name) assoc :docstring ~docstring)))))

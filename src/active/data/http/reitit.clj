(ns active.data.http.reitit
  (:require [active.data.translate.core :as translate]
            [active.data.translate.format :as format]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.http.common :as common]
            [active.data.realm :as realm]
            [reitit.coercion :as coercion]))

(defrecord ^:private RealmModel [realm open?])

(defn- wrap-coercion-errors [thunk]
  ;; turn format errors into coercion errors
  ;; Note: It seems as reitit would catch-all exceptions anyway, and swallow them quite silently.
  (try (thunk)
       (catch Exception e
         (cond
           (translate/format-error? e)
           (coercion/map->CoercionError
            {:problems [(ex-message e)]})

           :else
           (coercion/map->CoercionError
            {:problems [(.getMessage ^Exception e)]})))))

(defn- add-problems [coercion-error1 coercion-error2]
  (update coercion-error1 :problems concat (:problems coercion-error2)))

(defn- compile-model [model _name]
  ;; Note: model is what the user has given in the spec - may be {:foo realm} for query and path parameters, or any realm for bodies.
  (RealmModel. (if (and (map? model)
                        (not (realm-inspection/realm? model)))
                 (realm/map-with-keys model)
                 (realm/compile model))
               false))

(defn- convert-closed-map [format realm-map value]
  (reduce-kv (fn [res k realm]
               ;; TODO: add 'k' to the coercion error message
               ;; Note: if k is absent, v becomes nil, and if the realm is an optional it should pass => optional value realm means optional key.
               (let [v (get value k nil)
                     r (wrap-coercion-errors #((translate/from-extern realm format) v))]
                 (if (coercion/error? r)
                   (if (coercion/error? res)
                     (add-problems res r)
                     r)
                   (assoc res k r))))
             {}
             realm-map))

(defn- convert-map [format model value _format]
  ;; Note: _format can be 'application/transit+json' for example; not needed here.
  (let [realm (:realm model)
        open? (:open? model)]
    (assert (realm-inspection/map-with-keys? realm))
    (let [realm-map (realm-inspection/map-with-keys-realm-map realm)]
      ;; Note: we convert value-by-value, because the maps are implicit for path-params etc. The format cannot and should not decide how it looks like.
      (if open?
        ;; means that the value should be allowed to contain more keys than given. (afaik)
        (do
          (assert (realm-inspection/map-with-keys? realm) "Only map models can be open models.")
          (let [known-keys (keys realm-map)
                known (select-keys value known-keys)
                unconverted (if (empty? known-keys)
                              value
                              (apply dissoc value known-keys))
                converted (convert-closed-map format realm-map known)]
            (if (coercion/error? converted)
              converted
              (merge unconverted converted))))
        (convert-closed-map format realm-map value)))))

(defn- convert [format model value _format]
  ;; Note: _format can be 'application/transit+json' for example; not needed here.
  (assert (instance? RealmModel model) model)
  (let [realm (:realm model)
        open? (:open? model)]
    (assert (not open?)) ;; TODO: proper error (maybe allow, if realm is realm-with-keys map?)
    (wrap-coercion-errors (fn []
                            ((translate/from-extern realm format) value)))))

(defn realm-coercion
  "Returns a reitit coercion based on realms and the given realm formatter."
  ;; TODO: more docstring
  ;; Note: coercion comes after parsing (json, transit, something else)
  [body-format & {string-format :strings}]
  ;; see https://github.com/metosin/reitit/blob/ff99ab3ff929ca1b5fd7446d72d1a6eb07668795/modules/reitit-core/src/reitit/coercion.cljc#L39
  ;; for type/open/keywordize
  (let [string-format (or string-format common/default-string-format)]
    (reify coercion/Coercion
      (-get-name [_this] :active.data.http)
      (-get-options [_this] nil)
      ;; doesn't support apidocs yet (and maybe it can't)
      (-get-apidocs [_this _specification _data] nil)
      (-get-model-apidocs [_this _specificat _model _options] nil)
      (-compile-model [_this model name]
        ;; model will be a sequence of realms here, or maps for query/path params
        (assert (= 1 (count model)) "TODO: what do multiple models mean?")
        (->> model
             (map (fn [model]
                    (compile-model model name)))
             (first)))
      (-open-model [_this model]
        ;; this is called for query and form parameter coercion.
        (assert (instance? RealmModel model))
        (assoc model :open? true))
      (-encode-error [_this error]
        ;; error is the content of coercion/map->CoercionError here
        error)
      (-request-coercer [_this type model]
        ;; model is the result of compile-model
        ;; type should be :body or :string
        (case type
          :body (partial convert body-format model)
          :string (partial convert-map string-format model)))
      (-response-coercer [_this model]
        ;; model is the result of compile-model here
        (assert (instance? RealmModel model))
        (let [from (translate/to-extern (:realm model) body-format)]
          (fn [value _format]
            ;; Note: format can be 'application/transit+json' for example; not needed here.
            (wrap-coercion-errors #(from value))))))))

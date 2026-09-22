(ns active.data.http.json-schema
  (:require
   [active.data.http.realms :as realms]
   [active.data.realm :as realm]
   [active.data.realm.inspection :as inspection]))

(defn- unsupported-realm-error [realm]
  (ex-info (str "No json-schema representation for realm " (inspection/description realm))
           {:type ::unsupported-realm
            :realm realm}))

(defn unsupported-realm-error? [e]
  (= (:type (ex-data e)) ::unsupported-realm))

(defn- json-schema-from-realm*
  [as-key? realm]
  ;; Note: transit-json is a real superset of json, so it should be possible to support both here.
  (cond
    ;; Scalar realms

    ;; Note: number is weird, esp with respect to transit; not sure if it should be in here or not.
    (inspection/number? realm)
    {:type "number"}

    (realms/string-of-length-realm? realm)
    (let [[min max] (realms/string-of-length-realm? realm)]
      (if min
        {:type "string" :minLength min :maxLength max}
        {:type "string" :maxLength max}))

    (realms/iso-date-realm? realm)
    {:type "string" :format "date"}

    (or (realms/iso-offset-time-realm? realm)
        (realms/iso-time-realm? realm))
    ;; There are differences: offset-time is with offset, time is without offset.
    ;; But not sure json-schema can represent them.
    {:type "string" :format "time"}

    (or (realms/iso-offset-date-time-realm? realm)
        (realms/iso-date-time-realm? realm)
        (realms/iso-instant-realm? realm))
    ;; There are differences: offset-date-time is with offset, date-time is without offset, and instant is with Z offset only.
    ;; But not sure json-schema can represent them.
    {:type "string" :format "date-time"}

    (inspection/string? realm)
    {:type "string"}

    (inspection/char? realm)
    {:type "string" :pattern "~c."}

    (inspection/keyword? realm)
    {:type "string" :pattern "~:.*"}

    (inspection/symbol? realm)
    {:type "string" :pattern "~$.*"}

    (inspection/boolean? realm)
    (if as-key?
      {:enum ["~?t" "~?f"]}
      {:type "boolean"})

    (realms/uuid-string-realm? realm)
    {:type "string", :format "uuid"}

    (realms/transit-uuid-realm? realm)
    {:type "string", :pattern "~u[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"}

    ;; Covers integer, integer-from-to, integer-from, integer-to, natural.
    (inspection/integer-from-to? realm)
    (if as-key?
      {:type "string" :pattern "~i[-]?[0-9]+"}
      (merge {:type "integer"}
             (when-let [minimum (inspection/integer-from-to-realm-from realm)]
               {:minimum minimum})
             (when-let [maximum (inspection/integer-from-to-realm-to realm)]
               {:maximum maximum})))

    ;; Covers real-range, real.
    (inspection/real-range? realm)
    (if as-key?
      {:type "string" :pattern "~d[-]?[0-9]+.[0-9]*([eE][0-9]+)?"}
      (merge {:type "number"}
             (when-let [minimum (inspection/real-range-realm-left realm)]
               {(if (= :in (inspection/real-range-realm-clusive-left realm)) :minimum :exclusiveMinimum)
                minimum})
             (when-let [maximum (inspection/real-range-realm-right realm)]
               {(if (= :in (inspection/real-range-realm-clusive-right realm)) :minimum :exclusiveMaximum)
                maximum})))

    (inspection/optional? realm)
    (let [inner (inspection/optional-realm-realm realm)]
      {:oneOf [(json-schema-from-realm* as-key? inner)
               (if as-key?
                 {:const "~_"}
                 {:type "null"})]})

    (inspection/union? realm)
    (let [realms (inspection/union-realm-realms realm)
          json-schemas (mapv (partial json-schema-from-realm* as-key?) realms)]
      {:anyOf json-schemas})

    (and (inspection/enum? realm)
         (every? realms/json? (inspection/enum-realm-values realm)))
    {:enum (into [] (inspection/enum-realm-values realm))}

    (realms/list-of-realm? realm)
    {:type "array" :prefixItems [{:const "~#list"} {:type "array" :items (json-schema-from-realm* as-key? (inspection/sequence-of-realm-realm realm))}]}

    (inspection/sequence-of? realm) ;; when not a list
    {:type "array" :items (json-schema-from-realm* as-key? (inspection/sequence-of-realm-realm realm))}

    (inspection/set-of? realm)
    {:type "array" :prefixItems [{:const "~#set"} {:type "array" :uniqueItems true :items (json-schema-from-realm* as-key? (inspection/set-of-realm-realm realm))}]}

    (inspection/map-of? realm)
    {:type "array" :prefixItems [{:const "~#cmap"} {:type "array" :items
                                                    ;; it's actually key, value, key, value... but I think json-schema cannot express that.
                                                    {:anyOf [(json-schema-from-realm* true (inspection/map-of-realm-key-realm realm))
                                                             (json-schema-from-realm* false (inspection/map-of-realm-value-realm realm))]}}]}

    (inspection/map-with-keys? realm) ;; only if keys are strings? (or keywords, as for 'keywordize-keys')?
    (let [pairs (inspection/map-with-keys-realm-map realm)
          de-optional (fn [realm]
                        (if (inspection/optional? realm)
                          (inspection/optional-realm-realm realm)
                          realm))]
      {:type "object"
       :properties (->> pairs
                        (mapcat (fn [[k realm]] [k (json-schema-from-realm* as-key?
                                                                            (de-optional realm))]))
                        (apply array-map))
       :required (->> pairs
                      (filter (fn [[_k v]] (not (inspection/optional? v))))
                      (mapv first))
       :closed false})

    (inspection/map-with-tag? realm)
    {:type "object"
     :properties [[(inspection/map-with-tag-realm-key realm)
                   (json-schema-from-realm* as-key? (realm/enum (inspection/map-with-tag-realm-value realm)))]]
     ;; `map-with-tag` is open according to [[realm/contains?]].
     :closed false}

    (inspection/tuple? realm)
    {:type "array" :prefixItems (mapv (partial json-schema-from-realm* as-key?) (inspection/tuple-realm-realms realm)) :items false}

    (inspection/intersection? realm)
    {:allOf (mapv (partial json-schema-from-realm* as-key?) (inspection/intersection-realm-realms realm))}

    :else (do (assert (inspection/realm? realm) realm)
              (throw (unsupported-realm-error realm)))))

(defn json-schema-from-realm
  "Returns a json-schema description of the given realm, or throws [[unsupported-realm-error?]].
   Supports all json and transit-json realms, as well as some of the special realms in `active.data.http.realms` like `uuid-string` or `iso-date`."
  [realm]
  (json-schema-from-realm* false realm))

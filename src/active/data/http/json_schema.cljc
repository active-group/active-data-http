(ns active.data.http.json-schema
  (:require
   [active.data.realm :as realm]
   [active.data.realm.inspection :as inspection]
   [active.data.realm.internal.records :as internal-records])
  #?(:clj (:import
           [java.lang UnsupportedOperationException])))

(defn- throw-unsupported-realm [realm]
  (let [msg (str "no json-schema representation for realm " (pr-str realm))]
    #?(:clj (throw (UnsupportedOperationException. msg))
       :cljs (throw (js/Error. msg)))))

(defn format->json-schema-from-realm [format]
  ;; format = transit-basic ~ application/transit
  ;; format = json-basic ~ application/json
  ;; format = xml-basic ~ application/xml
  (fn [realm]
    ...))

{:format ... ; <-
 :content-type ...
 :json-schema ...}

(defn json-schema-from-realm [realm]
  (cond
    ;; Scalar realms
    (or (inspection/rational? realm)
        (inspection/number? realm))
    {:type "number"}

    (or (inspection/char? realm)
        (inspection/keyword? realm)
        (inspection/symbol? realm)
        (inspection/string? realm))
    {:type "string"}

    (inspection/boolean? realm)
    {:type "boolean"}

    (inspection/uuid? realm)
    {:type "string", :format "uuid"}

    (inspection/any? realm)
    {}

    ;; Covers integer, integer-from-to, integer-from, integer-to, natural.
    (inspection/integer-from-to? realm)
    (merge {:type "integer"}
           (when-let [minimum (inspection/integer-from-to-realm-from realm)]
             {:minimum minimum})
           (when-let [maximum (inspection/integer-from-to-realm-to realm)]
             {:maximum maximum}))

    ;; Covers real-range, real.
    (inspection/real-range? realm)
    (merge {:type "number"}
           (when-let [minimum (inspection/real-range-realm-left realm)]
             {(if (= :in (inspection/real-range-realm-clusive-left realm)) :minimum :exclusiveMinimum)
              minimum})
           (when-let [maximum (inspection/real-range-realm-right realm)]
             {(if (= :in (inspection/real-range-realm-clusive-right realm)) :minimum :exclusiveMaximum)
              maximum}))

    (inspection/optional? realm)
    (let [inner (inspection/optional-realm-realm realm)]
      {:oneOf [(json-schema-from-realm inner) {:type "null"}]})

    (inspection/union? realm)
    (let [realms (inspection/union-realm-realms realm)
          json-schemas (map json-schema-from-realm realms)]
      {:anyOf json-schemas})

    (inspection/enum? realm)
    ;; TODO: Value can be anything, but not anything is a valid json-schema
    ;; value.
    {:enum (into [] (inspection/enum-realm-values realm))}

    (inspection/intersection? realm)
    {:allOf (map json-schema-from-realm (inspection/intersection-realm-realms realm))}

    (inspection/sequence-of? realm)
    {:type "array" :items (json-schema-from-realm (inspection/sequence-of-realm-realm realm))}

    (inspection/set-of? realm)
    {:type "array" :uniqueItems true :items (json-schema-from-realm (inspection/set-of-realm-realm realm))}

    (inspection/map-with-keys? realm)
    (let [pairs (inspection/map-with-keys-realm-map realm)]
      {:type "object"
       :properties (->> pairs
                        (mapcat (fn [[k realm]] [k (json-schema-from-realm realm)]))
                        (apply array-map))
       ;; :required (mapv first pairs)
       ;; Alternatively, treat keys of optional realms as not required.
       :required (->> pairs
                      (filter (fn [[_k v]] (not (inspection/optional? v))))
                      (mapv first))

       ;; TODO Always closed? Makes sense for realms, but maybe not in
       ;; general (for that usecase, think `:form` parameters that need to be
       ;; open).
       :closed true})

    (inspection/map-with-tag? realm)
    {:type "object"
     :properties [[(inspection/map-with-tag-realm-key realm)
                   (json-schema-from-realm (realm/enum (inspection/map-with-tag-realm-value realm)))]]
     ;; `map-with-tag` is open according to [[realm/contains?]].
     :closed false}

    (inspection/tuple? realm)
    {:type "array" :prefixItems (mapv json-schema-from-realm (inspection/tuple-realm-realms realm)) :items false}

    #_#_(inspection/record? realm)
    (let [fields (inspection/record-realm-fields realm)]
      {:type "object"
       :properties (->> fields
                        (mapcat (fn [record-realm-field]
                                  [(internal-records/record-realm-field-name record-realm-field)
                                   (json-schema-from-realm (internal-records/record-realm-field-realm record-realm-field))]))
                        (apply array-map))
       :required (mapv internal-records/record-realm-field-name fields)
       ;; Record always have to be closed(?).
       :closed true})
    ;; TODO from-predicate, function, delay, named.
    :else (throw-unsupported-realm realm)))

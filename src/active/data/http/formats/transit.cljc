(ns active.data.http.formats.transit
  (:require [active.data.translate.format :as format]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.clojure.lens :as lens]
            #?(:cljs [cognitect.transit :as transit])))

(def ^:private transit?
  ;; Note: this can be an expensive call; don't use it too much.
  #?(:cljs (some-fn nil?
                    keyword?
                    string? ;; = char
                    boolean?
                    symbol?
                    #(instance? js/Date %)
                    transit/bigdec? ;; = decimal?
                    transit/bigint?
                    transit/binary?
                    transit/integer?
                    transit/link?
                    transit/quoted?
                    ;; transit/tagged-value?
                    transit/uri?
                    transit/uuid?

                    ;; TODO array, list, set, map, cmap?
                    )
     :clj (some-fn nil?
                   keyword?
                   string?
                   boolean?
                   integer? ;; is that correct?
                   decimal? ;; is that correct?
                   symbol?
                   ;; TODO bigdec, bigint
                   #(instance? java.util.Date %)
                   uri?
                   uuid?
                   char?
                   ;; TODO array, list, set, map, link
                   )))

(def ^:private id lens/id)

(def ^:private transit-uuid
  ;; Note: realm uuid is: clojure.core/uuid? (java.util.UUID) resp. cljs.core/uuid? (cljs.core/UUID)
  ;; transit uuid is: java.util.UUID  resp. com.cognitect.transit in cljs
  #?(:clj id
     :cljs (lens/xmap (fn to-realm [v]
                        ;; v should satisfy (transit/uuid? v)
                        (uuid (str v)))
                      (fn from-realm [v]
                        (transit/uuid (str v))))))

(defn- optional [lens]
  (lens/xmap (fn to-realm [v]
               (when (some? v) (lens/yank v lens)))
             (fn from-realm [v]
               (when (some? v) (lens/shove nil lens v)))))

(defn- set-as-seq "Turns a lens that works on sequences to a lens that works on sets." [seq-lens]
  (lens/xmap (fn to-realm [v]
               (set (lens/yank v seq-lens)))
             (fn from-realm [v]
               (set (lens/shove nil seq-lens v)))))

(def ^:private nil-as-empty-map
  (lens/xmap (fn to-realm [v]
               (if (nil? v)
                 {}
                 v))
             (fn from-realm [v]
               (if (nil? v)
                 {}
                 v))))

#_(def ^:private transit-number
    #?(:clj id
       :cljs (lens/xmap (fn to-realm [v]
                        ;; js/Number or gm/Long to js/Number ?

                        ;; v should satisfy (transit/uuid? v)
                          (uuid (str v)))
                        (fn from-realm [v]
                          (transit/uuid (str v))))))

;; transit-realm ? the things that can be represented unambiguously
#_(def transit-realm
    (realm/union realm/string
                 (-> realm/any
                     (realm/restricted transit?))))

(defn- tagged-union-lens [realms recurse]
  ;; represents (union r1 r2) as [0 ->r1] [1 ->r2] etc.
  (let [tags-realms-lenses-map (->> realms
                                    (map-indexed (fn [idx realm]
                                                   [idx [realm (recurse realm)]]))
                                    (into {}))]
    (lens/xmap (fn to-realm [raw]
                 (when (not (and (vector? raw)
                                 (= 2 (count raw))))
                   (throw (format/format-error "Expected a vector of length 2 as the union representation" raw)))
                 (let [[idx v] raw]
                   (if-let [[_realm lens] (get tags-realms-lenses-map idx)]
                     (lens/yank v lens)
                     (throw (format/format-error "Unexpected union tag" idx)))))
               (let [try-all (->> tags-realms-lenses-map
                                  (map (fn [[idx [realm lens]]]
                                         (fn [value]
                                           (when (realm/contains? realm value)
                                             [idx (lens/shove nil lens value)]))))
                                  (apply some-fn))]
                 (fn from-realm [v]
                   (or (try-all v)
                       (throw (format/format-error "Value not contained in union realm" v))))))))

;; (defn flat-union ... just try them all)
#_(defn- flat-union [realms recurse]
  ;; represents (union r1 r2) as r1 | r2, just trying all
    (let [lenses (doall (map recurse realms))]
      (lens/xmap (fn to-realm [v]
                   (let [res (reduce (fn [_ lens]
                                       (try (reduced [(lens/yank v lens)])
                                          ;; TODO: format-error
                                            (catch #?(:clj Exception :cljs :default) e
                                              nil)))
                                     nil
                                     lenses)]
                     (if (nil? res)
                       (throw ...) ;; TODO format-error
                       (first res))))
                 (fn from-realm [v]
                   (let [res (reduce (fn [_ lens]
                                       ;; could use (realm/contains?) here to reduce attempts
                                       (try (reduced [(lens/shove nil lens v)])
                                            (catch #?(:clj Exception :cljs :default) e
                                              nil)))
                                     nil
                                     lenses)]
                     (if (nil? res)
                       (throw ...)
                       (first res)))))))

(defn- flat-record-lens [realm recurse]
  ;; represents a record as a vector of the values, in the order defined by the record realm.
  (let [realms (map realm-inspection/record-realm-field-realm
                    (realm-inspection/record-realm-fields realm))
        lenses (doall (map recurse realms))
        getters  (map realm-inspection/record-realm-field-getter
                      (realm-inspection/record-realm-fields realm))
        constr (realm-inspection/record-realm-constructor realm)]
    (lens/xmap (fn [edn]
                 (when-not (vector? edn)
                   (throw (format/format-error "Expected a vector for record realm" edn)))
                 (when (not= (count (realm-inspection/record-realm-fields realm))
                             (count edn))
                   (throw (format/format-error (str "Expected " (count (realm-inspection/record-realm-fields realm)) " values for record realm, but only got " (count edn))
                                               edn)))
                 (let [vals (map (fn [v lens]
                                   (lens/yank v lens))
                                 edn
                                 lenses)
                       res (apply constr vals)]
                   res))
               (fn [inst]
                 (let [res (let [vals (map (fn [getter]
                                             (getter inst))
                                           getters)
                                 edn (map (fn [v lens]
                                            (lens/shove nil lens v))
                                          vals
                                          lenses)]
                             (vec edn))]
                   res)))))

#_(defn- tagged-record-lens [realm recurse]
    (transit/tagged-value (record-realm-name ...)
                          {field-name => (recurse ...) value}))

(defn- ensure-transit [_realm pred lens]
  (lens/xmap (fn to-realm [v]
               ;; TODO: generate more helpful messages.
               (when-not (pred v)
                 (throw (format/format-error "Unexpected value" v)))
               (lens/yank v lens))
             (fn from-realm [v]
               (lens/shove nil lens v))))

(defn- map-lens [key-lens value-lens]
  (lens/xmap (fn to-realm [v]
               (persistent! (reduce-kv (fn [res k v]
                                         (assoc! res (lens/yank k key-lens) (lens/yank v value-lens)))
                                       (transient {}) v)))
             (fn from-realm [v]
               (persistent! (reduce-kv (fn [res k v]
                                         (assoc! res (lens/shove nil key-lens k) (lens/shove nil value-lens v)))
                                       (transient {}) v)))))

(defn- vector-lens [lenses]
  ;; TODO: there are probably more efficient ways
  (lens/>> (lens/default (vec (repeat (count lenses) nil)))
           (lens/pattern (->> lenses
                              (map-indexed (fn [idx lens]
                                             (lens/>> (lens/at-index idx) lens)))
                              (vec)))))

(defn- ensure-map-has-no-other-keys [realm lens]
  (let [expected-key? (set (keys (realm-inspection/map-with-keys-realm-map realm)))]
    (lens/xmap (fn to-realm [v]
                 ;; TODO: generate more helpful messages.
                 (when-not (map? v)
                   (throw (format/format-error "Not a map" v)))
                 (doseq [[k _v] v]
                   (when-not (expected-key? k)
                     (throw (format/format-error "Unexpected key in map" k))))
                 (lens/yank v lens))
               (fn from-realm [v]
                 (lens/shove nil lens v)))))

(defn basic [realm]
  ;; Note: this does some checks on the transit values that are read,
  ;; but those checks only guarantee that no information is lost/silently dropped.
  ;; The translated values may still not be 'contained' in the target
  ;; realm, which has to be checked separately.
  (fn [recurse] ;; TODO: move that down to where it is needed
    (cond
      (realm-inspection/optional? realm)
      (let [inner-t (recurse (realm-inspection/optional-realm-realm realm))]
        (optional inner-t))

      (realm-inspection/delayed? realm)
      ;; Note: for now we assume, that at the time this translation is fetched, the realm must be resolvable.
      (recurse (deref (realm-inspection/delayed-realm-delay realm)))

      (realm-inspection/named? realm)
      (recurse (realm-inspection/named-realm-realm realm))

      ;; (realm-inspection/union? realm) ;; or flat-union? try them all? maybe not.

      ;; Note: because every value should conform to all intersected realms, every translation should be able to translate all values.
      ;; So we can just take the first one. (can't be empty)
      (realm-inspection/intersection? realm)
      ;; We could also try them all and use the first that works?
      (recurse (first (realm-inspection/intersection-realm-realms realm)))

      ;; TODO: what about map-with-tag?
      (realm-inspection/builtin-scalar? realm)
      (case (realm-inspection/builtin-scalar-realm-id realm)
        :number id ;; TODO: is this correct?
        :keyword id
        :symbol id
        :string id
        :boolean id
        :uuid transit-uuid
        ;; TODO: can we support :char ? :rational?
        ;; :any id ;; assuming the value is compatible with transit. (do runtime check?; offer support for transit-realm instead of any?)
        (throw (format/unsupported-exn realm)))

      ;; TODO: support transit-realm, edn-realm?

      (realm-inspection/integer-from-to? realm)
      id

      (realm-inspection/real-range? realm)
      id

      (realm-inspection/sequence-of? realm)
      (ensure-transit realm sequential?
                      (lens/mapl (recurse (realm-inspection/sequence-of-realm-realm realm))))

      (realm-inspection/set-of? realm)
      (ensure-transit realm set?
                      (set-as-seq (lens/mapl (recurse (realm-inspection/set-of-realm-realm realm)))))

      (realm-inspection/map-with-keys? realm)
      (ensure-map-has-no-other-keys
       realm
       (lens/pattern (->> (realm-inspection/map-with-keys-realm-map realm)
                          (map (fn [[k value-realm]]
                                 (when-not (transit? k)
                                   (throw (format/unsupported-exn k [realm])))
                                 [(lens/member k) (lens/>> (lens/member k) (recurse value-realm))]))
                          (into {}))))

      (realm-inspection/map-of? realm)
      (ensure-transit realm map?
                      (map-lens (recurse (realm-inspection/map-of-realm-key-realm realm))
                                (recurse (realm-inspection/map-of-realm-value-realm realm))))

      (realm-inspection/tuple? realm)
      (ensure-transit realm #(and (vector? %)
                                  (= (count %) (count (realm-inspection/tuple-realm-realms realm))))
                      (vector-lens (doall (map recurse (realm-inspection/tuple-realm-realms realm)))))

      (realm-inspection/map-with-tag? realm)
      ;; Note: we can check that the speficic key and value are transit?, but we have to assume the rest of the map is too. Can't help with translation.
      (let [k (realm-inspection/map-with-tag-realm-key realm)
            tag (realm-inspection/map-with-tag-realm-value realm)]
        (when-not (transit? k)
          (throw (format/unsupported-exn {:key k})))
        (when-not (transit? tag)
          (throw (format/unsupported-exn {:value tag})))
        (ensure-transit realm map?
                        lens/id))

      (realm-inspection/enum? realm)
      (let [values (realm-inspection/enum-realm-values realm)]
        (doseq [v values]
          (when-not (transit? v)
            (throw (format/unsupported-exn v))))
        lens/id)

      :else
      (throw (format/unsupported-exn realm)))))

(defn- extended [realm]
  ;; Note: this does some checks on the transit values that are read,
  ;; but those checks only guarantee that no information is lost/silently dropped.
  ;; The translated values may still not be 'contained' in the target
  ;; realm, which has to be checked separately.
  (fn [recurse] ;; TODO: move that down to where it is needed
    (cond
      (realm-inspection/union? realm)
      ;; or flat-union? flat-union has a smaller representation, but will have less performance.
      (tagged-union-lens (realm-inspection/union-realm-realms realm) recurse)

      (realm-inspection/builtin-scalar? realm)
      (case (realm-inspection/builtin-scalar-realm-id realm)
        :any id ;; assuming the value is compatible with transit. (do runtime check?; offer support for transit-realm instead of any?)
        ((basic realm) recurse))

      ;; TODO: support transit-realm, edn-realm?

      (realm-inspection/record? realm)
      (flat-record-lens realm recurse)

      :else
      ((basic realm) recurse))))

(def ^{:doc "Translates values described by a realm to values usable by transit. The defaults cover most realms."} transit-format
  ;; Note: use this only when you are ok with the coupling that this introduces.

  ;; Coupling can for example be
  ;; - between producer and consumer code of the transit values, if
  ;;   they are developed independently
  ;; - between past and future versions of the code, if transit values
  ;;   are written to databases, or if producer and consumer can have different
  ;;   versions of the code.
  ;;
  ;; To prevent that, define translators for every realm that you
  ;; expect to change over time, or all of them to be sure. Then make
  ;; those definitions forwards/backwards-compatible to the extend possible or
  ;; needed, or expect different versions of the data.

  ;; TODO: offer checked/unchecked versions
  (format/format ::transit
                 extended))

(def transit-basic-format
  (format/format ::transit-basic
                 basic))

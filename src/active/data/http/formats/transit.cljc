(ns active.data.http.formats.transit
  (:require [active.data.translate.format :as format]
            [active.data.translate.formatter :as formatter]
            [active.data.realm.inspection :as realm-inspection]
            [active.clojure.lens :as lens]
            #?(:cljs [cognitect.transit :as transit])
            [active.data.realm :as realm]))

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

(def ^:private transit-uuid
  ;; Note: realm uuid is: clojure.core/uuid? (java.util.UUID) resp. cljs.core/uuid? (cljs.core/UUID)
  ;; transit uuid is: java.util.UUID  resp. com.cognitect.transit in cljs
  #?(:clj lens/id
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

#_(defn- flat-record [realm]
    (fn [resolve]
    ;; represents a record as a vector of the values, in the order defined by the record realm.
      (let [realms (map realm-inspection/record-realm-field-realm
                        (realm-inspection/record-realm-fields realm))
            lenses (doall (map resolve realms))
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
                                     ;; TODO: errors-at pos/key...
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
                                              ;; TODO: (errors-at getter )
                                                (lens/shove nil lens v))
                                              vals
                                              lenses)]
                                 (vec edn))]
                       res))))))

(defn- record-as-tuple [record-realm tuple-lens]
  (let [getters (map realm-inspection/record-realm-field-getter
                     (realm-inspection/record-realm-fields record-realm))
        ctor (realm-inspection/record-realm-constructor record-realm)]
    (lens/xmap (fn to-realm [v]
                 (apply ctor (lens/yank v tuple-lens)))
               (fn from-realm [v]
                 (lens/shove nil tuple-lens
                             (mapv (fn [getter]
                                     (getter v))
                                   getters))))))

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
  ;; TODO: there are probably more efficient ways to implement this
  ;; Note: lens/default would turn an empty vector into nil
  (if (empty? lenses)
    (lens/xmap (fn to-realm [v]
                 (when-not (= [] v)
                   (throw (format/format-error "Not an empty vector" v)))
                 [])
               (fn from-realm [v]
                 (when-not (= [] v)
                   (throw (format/format-error "Not an empty vector" v)))
                 []))
    (lens/>> (lens/default (vec (repeat (count lenses) nil)))
             (lens/pattern (->> lenses
                                (map-indexed (fn [idx lens]
                                               (lens/>> (lens/at-index idx) lens)))
                                (vec))))))

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

(defn- doall-lens [lens]
  (lens/lens (fn to-realm [v]
               (doall (lens/yank v lens)))
             (fn from-realm [d v]
               (doall (lens/shove d lens v)))))

(defn- map-member [k]
  ;; Note: lens/member would do a dissoc on shoving nil; we don't want that.
  (lens/lens (fn to-realm [v]
               (get v k))
             (fn from-realm [m v]
               (assoc m k v))))

(def basic-formatters
  (fn [realm]
    ;; Note: this does some checks on the transit values that are read,
    ;; but those checks only guarantee that no information is lost/silently dropped.
    ;; The translated values may still not be 'contained' in the target
    ;; realm, which has to be checked separately if needed.
    (cond
      (realm-inspection/optional? realm)
      (fn [resolve]
        (let [inner-t (resolve (realm-inspection/optional-realm-realm realm))]
          (optional inner-t)))

      (realm-inspection/delayed? realm)
      ;; Note: for now we assume, that at the time this translation is fetched, the realm must be resolvable.
      (fn [resolve]
        (resolve (deref (realm-inspection/delayed-realm-delay realm))))

      (realm-inspection/named? realm)
      (fn [resolve]
        (resolve (realm-inspection/named-realm-realm realm)))

      ;; (realm-inspection/union? realm) ;; or flat-union? try them all? maybe not.

      ;; Note: because every value should conform to all intersected realms, every translation should be able to translate all values.
      ;; So we can just take the first one. (can't be empty)
      (realm-inspection/intersection? realm)
      ;; We could also try them all and use the first that works?
      (fn [resolve]
        (resolve (first (realm-inspection/intersection-realm-realms realm))))

      (realm-inspection/builtin-scalar? realm)
      (case (realm-inspection/builtin-scalar-realm-id realm)
        :number formatter/id ;; TODO: is this correct? transit-number?
        :keyword formatter/id
        :symbol formatter/id
        :string formatter/id
        :boolean formatter/id
        :uuid (formatter/simple transit-uuid)
        ;; TODO: can we support :char ? :rational?
        ;; :any id ;; assuming the value is compatible with transit. (do runtime check?; offer support for transit-realm instead of any?)
        (throw (format/unsupported-exn realm)))

      ;; TODO: support transit-realm, edn-realm?

      (realm-inspection/integer-from-to? realm)
      formatter/id

      (realm-inspection/real-range? realm)
      formatter/id

      (realm-inspection/sequence-of? realm)
      (fn [resolve]
        (ensure-transit realm sequential?
                        (doall-lens (lens/mapl (resolve (realm-inspection/sequence-of-realm-realm realm))))))

      (realm-inspection/set-of? realm)
      (fn [resolve]
        (ensure-transit realm set?
                        (set-as-seq (lens/mapl (resolve (realm-inspection/set-of-realm-realm realm))))))

      (realm-inspection/map-with-keys? realm)
      (fn [resolve]
        (ensure-map-has-no-other-keys
         realm
         (lens/pattern (->> (realm-inspection/map-with-keys-realm-map realm)
                            (map (fn [[k value-realm]]
                                   (when-not (transit? k)
                                     (throw (format/unsupported-exn k [realm])))
                                   [(map-member k) (lens/>> (map-member k) (resolve value-realm))]))
                            (into {})))))

      (realm-inspection/map-of? realm)
      (fn [resolve]
        (ensure-transit realm map?
                        (map-lens (resolve (realm-inspection/map-of-realm-key-realm realm))
                                  (resolve (realm-inspection/map-of-realm-value-realm realm)))))

      (realm-inspection/tuple? realm)
      (fn [resolve]
        (ensure-transit realm #(and (vector? %)
                                    (= (count %) (count (realm-inspection/tuple-realm-realms realm))))
                        (vector-lens (doall (map resolve (realm-inspection/tuple-realm-realms realm))))))

      (realm-inspection/map-with-tag? realm)
      ;; Note: we can check that the speficic key and value are transit?, but we have to assume the rest of the map is too. Can't help with translation.
      (let [k (realm-inspection/map-with-tag-realm-key realm)
            tag (realm-inspection/map-with-tag-realm-value realm)]
        (when-not (transit? k)
          (throw (format/unsupported-exn {:key k})))
        (when-not (transit? tag)
          (throw (format/unsupported-exn {:value tag})))
        (formatter/simple (ensure-transit realm map? ;; TODO: no check for the tag?
                                          lens/id)))

      (realm-inspection/enum? realm)
      (let [values (realm-inspection/enum-realm-values realm)]
        ;; if the values are transit, we can format them as-is.
        (doseq [v values]
          (when-not (transit? v)
            (throw (format/unsupported-exn v))))
        formatter/id)

      ;; TODO: support some kind of explicit transit-realm, edn-realm?

      :else
      (throw (format/unsupported-exn realm)))))

(defn- extended [realm]
  ;; Note: this does some checks on the transit values that are read,
  ;; but those checks only guarantee that no information is lost/silently dropped.
  ;; The translated values may still not be 'contained' in the target
  ;; realm, which has to be checked separately.
  (cond
    (realm-inspection/union? realm)
    ;; simply use the order/position of the sub-realm as a tag
    (formatter/tagged-union-tuple (->> (realm-inspection/union-realm-realms realm)
                                       (map-indexed vector)
                                       (into {})))

    (realm-inspection/builtin-scalar? realm)
    (case (realm-inspection/builtin-scalar-realm-id realm)
      :any formatter/id ;; assuming the value is compatible with transit. (do runtime check?; offer support for transit-realm instead of any?)
      (basic-formatters realm))

    (realm-inspection/record? realm)
    #_(flat-record realm)
    ;; based on the tuple implementation
    (let [base (basic-formatters (apply realm/tuple (map realm-inspection/record-realm-field-realm
                                                         (realm-inspection/record-realm-fields realm))))]
      (fn [recurse]
        (record-as-tuple realm (base recurse))))

    :else
    (basic-formatters realm)))

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

  ;; TODO: offer checked/unchecked versions?
  (format/format ::transit
                 extended))

#_(def transit-basic-format
    (format/format ::transit-basic
                   basic))

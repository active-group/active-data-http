(ns active.data.http.realms
  "Realms that can be useful in web programming. Some of these also take
   special roles in the rest of this library."
  (:require
   [active.data.realm :as realm #?@(:cljs [:include-macros true])]
   [active.data.realm.inspection :as realm-inspection]
   #?(:cljs [cognitect.transit :as transit]))
  (:refer-clojure :exclude [vector-of time])
  #?(:clj (:import (java.time LocalDate OffsetDateTime OffsetTime Instant LocalTime LocalDateTime))))

;; TODO: These might be useful, too:  ipv4, ipv6, uri, email
;; TODO? string with pattern; and more formats (https://json-schema.org/understanding-json-schema/reference/type#built-in-formats)

(defn- restricted-realm-pred [base? v]
  (when (and (realm-inspection/intersection? v)
             (= 2 (count (realm-inspection/intersection-realm-realms v))))
    (let [[a b] (realm-inspection/intersection-realm-realms v)]
      (when (and (base? a)
                 (realm-inspection/from-predicate? b))
        (realm-inspection/predicate b)))))

(defn- restricted-realm?
  [base? predicate v]
  (= (restricted-realm-pred base? v)
     predicate))

(defrecord ^:private LengthRestriction [min max]
  #?@(:clj [clojure.lang.IFn
            (invoke [_this v] (>= (or min 0) (count v) max))]
      :cljs [IFn
             (-invoke [_this v] (>= (or min 0) (count v) max))]))

(defn string-of-length
  "Realm of strings with a given maximum and optional minimum length."
  ([max]
   (string-of-length nil max))
  ([min max]
   (realm/restricted realm/string
                     ;; using an IFn to make it reflectable (metadata could be an option too)
                     (LengthRestriction. min max)
                     (if min
                       (str "length between " min " and " max)
                       (str "length up to " max)))))

(defn string-of-length-realm? "Returns [min max] if r is a string-of-length realm" [r]
  (when-let [p (restricted-realm-pred realm-inspection/string? r)]
    (when (instance? LengthRestriction p)
      [(:min p) (:max p)])))

(defn uuid-string? [s]
  (and (string? s)
       (some? (parse-uuid s))))

(def ^{:doc "Realm of strings that represent a UUID."}
  uuid-string (realm/restricted realm/string
                                uuid-string? "uuid string"))

(defn uuid-string-realm? [realm]
  (restricted-realm? realm-inspection/string? uuid-string? realm))

;; date and time

#?(:clj
   ;; No cljs support for now, as js/Date is so messed up with respect to timezones.
   [(defn- local-date? [v]
      (instance? LocalDate v))

    (def local-date (realm/from-predicate "local date"
                                          local-date?))

    (defn local-date-realm? [r]
      (and (realm-inspection/from-predicate? r)
           (= local-date? (realm-inspection/predicate r))))

    ;; could add local-date-time and local-time?

    (defn- local-time? [v]
      (instance? LocalTime v))

    (def local-time (realm/from-predicate "local time"
                                          local-time?))

    (defn local-time-realm? [r]
      (and (realm-inspection/from-predicate? r)
           (= local-time? (realm-inspection/predicate r))))

    (defn- local-date-time? [v]
      (instance? LocalDateTime v))

    (def local-date-time (realm/from-predicate "local date and time"
                                               local-date-time?))

    (defn local-date-time-realm? [r]
      (and (realm-inspection/from-predicate? r)
           (= local-date-time? (realm-inspection/predicate r))))

    (defn- offset-date-time? [v]
      (instance? OffsetDateTime v))

    (def offset-date-time (realm/from-predicate "offset date and time"
                                                offset-date-time?))

    (defn offset-date-time-realm? [r]
      (and (realm-inspection/from-predicate? r)
           (= offset-date-time? (realm-inspection/predicate r))))

    (defn- offset-time? [v]
      (instance? OffsetTime v))

    (def offset-time (realm/from-predicate "offset time"
                                           offset-time?))

    (defn offset-time-realm? [r]
      (and (realm-inspection/from-predicate? r)
           (= offset-time? (realm-inspection/predicate r))))])

(defn- instant? [v]
  ;; Note: js/Date does not store timezone information, so it's most like Instant.
  #?(:clj (instance? Instant v)
     :cljs (instance? js/Date v)))

(def instant (realm/from-predicate "instant in time"
                                   instant?))

(defn instant-realm? [r]
  (and (realm-inspection/from-predicate? r)
       (= instant? (realm-inspection/predicate r))))

;; date and time strings: https://datatracker.ietf.org/doc/html/rfc3339#section-5.6

(defn iso-instant? [s]
  ;; like offset date time, but only Zulu time allowed
  (boolean (and (string? s)
                (re-matches #"^(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d\.\d+Z)|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\dZ)|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\dZ)$"
                            s))))

(defn iso-date? [s]
  (boolean (and (string? s)
                (re-matches #"^(\d{4}-[01]\d-[0-3]\d)$" s))))

(defn iso-offset-time? [s]
  (boolean (and (string? s)
                (re-matches #"^([0-2]\d:[0-5]\d:[0-5]\d([+-][0-2]\d:[0-6]\d|Z))|([0-2]\d:[0-5]\d([+-][0-2]\d:[0-6]\d|Z))$" s))))

(defn iso-offset-date-time? [s]
  (boolean (and (string? s)
                (re-matches #"^(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d\.\d+([+-][0-2]\d:[0-6]\d|Z))|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d([+-][0-2]\d:[0-5]\d|Z))|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d([+-][0-2]\d:[0-6]\d|Z))$"
                            s))))

(defn iso-time? [s]
  ;; without timezone!
  (boolean (and (string? s)
                (re-matches #"^[0-2]\d:[0-5]\d:[0-5]\d$" s))))

(defn iso-date-time? [s]
  ;; without timezone!
  (boolean (and (string? s)
                (re-matches #"^(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d\.\d+)|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d)|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d)$"
                            s))))

(def ^{:doc "Realm of strings representing an ISO instant in time"} iso-instant
  (realm/restricted realm/string iso-instant? "ISO instant"))

(defn iso-instant-realm? [realm]
  (restricted-realm? realm-inspection/string? iso-instant? realm))

(def ^{:doc "Realm of strings representing an ISO date"} iso-date
  ;; a (local) date, without timezone
  (realm/restricted realm/string iso-date? "ISO date"))

(defn iso-date-realm? [realm]
  (restricted-realm? realm-inspection/string? iso-date? realm))

(def ^{:doc "Realm of strings representing an ISO time with timezone offset"} iso-offset-time
  (realm/restricted realm/string iso-offset-time? "ISO offset time"))

(defn iso-offset-time-realm? [realm]
  (restricted-realm? realm-inspection/string? iso-offset-time? realm))

(def ^{:doc "Realm of strings representing an ISO date and time with timezone offset"} iso-offset-date-time
  ;; a date and time with timezone
  (realm/restricted realm/string iso-offset-date-time? "ISO offset date-time"))

(defn iso-offset-date-time-realm? [realm]
  (restricted-realm? realm-inspection/string? iso-offset-date-time? realm))

(def ^{:doc "Realm of strings representing an ISO time without timezone offset"} iso-time
  (realm/restricted realm/string iso-time? "ISO time"))

(defn iso-time-realm? [realm]
  (restricted-realm? realm-inspection/string? iso-time? realm))

(def ^{:doc "Realm of strings representing an ISO date and time without timezone offset"} iso-date-time
  (realm/restricted realm/string iso-date-time? "ISO date-time"))

(defn iso-date-time-realm? [realm]
  (restricted-realm? realm-inspection/string? iso-date-time? realm))

;;

(defn list-of "Realm of lists with items of the given realm." [item-realm]
  (realm/restricted (realm/sequence-of item-realm)
                    list? "list"))

(defn list-of-realm? [realm]
  (restricted-realm? realm-inspection/sequence-of?
                     list?
                     realm))

(defn vector-of "Realm of vectors with items of the given realm." [item-realm]
  (realm/restricted (realm/sequence-of item-realm)
                    vector? "vector"))

(defn vector-of-realm? [realm]
  (restricted-realm? realm-inspection/sequence-of?
                     vector?
                     realm))

;; json ******************************

(def ^{:doc "The realm of json values."} json
  ;; or actually: clojure values, which are typically associated with corresponding json values.
  (realm/union realm/string
               realm/char
               realm/boolean
               realm/number
               realm/integer
               realm/real
               (realm/enum nil)
               (realm/map-of (realm/union realm/string realm/keyword)
                             (realm/delay json))
               (vector-of (realm/delay json))))

(defn json? [v]
  ;; Note: 'map-of' does not check much; but maybe it should?
  (realm/contains? json v))

;; public because transit uses it too
(defn ^:no-doc json-realm-of [rec-var value-pred?]
  (realm/union realm-inspection/string
               realm-inspection/boolean
               realm-inspection/number
               realm-inspection/integer-from-to
               realm-inspection/real-range

               (realm/restricted realm-inspection/optional
                                 (fn [r]
                                   (realm/contains? @rec-var (realm-inspection/optional-realm-realm r)))
                                 "optional json")

               ;; (realms/vector-of json-realm) must be 'inlined' here because of recursion
               (realm/restricted realm-inspection/intersection
                                 (fn [r]
                                   (and (= 2 (count (realm-inspection/intersection-realm-realms r)))
                                        (let [[a b] (realm-inspection/intersection-realm-realms r)]
                                          (and (realm-inspection/sequence-of? a)
                                               (realm/contains? @rec-var (realm-inspection/sequence-of-realm-realm a))
                                               (realm-inspection/from-predicate? b)
                                               (= vector? (realm-inspection/predicate b))))))
                                 "vector-of of json")

               (realm/restricted realm-inspection/map-with-keys
                                 ;; if keys are strings (or keywords, allowing for 'keywordize-keys'), and values are json
                                 (fn [r]
                                   (let [m (realm-inspection/map-with-keys-realm-map r)]
                                     (and (every? #(or (string? %) (keyword? %)) (keys m))
                                          (every? #(realm/contains? @rec-var %) (vals m)))))
                                 "map of json")

               (realm/restricted realm-inspection/map-with-tag
                                 ;; if tags are strings (or keywords), and value is json
                                 (fn [r]
                                   (let [tag (realm-inspection/map-with-tag-realm-key r)
                                         value (realm-inspection/map-with-tag-realm-value r)]
                                     (and (or (string? tag) (keyword? tag))
                                          (@value-pred? value))))
                                 "map with json tag")

               (realm/restricted realm-inspection/map-of
                                 (fn [r]
                                   (and (or (realm-inspection/string? (realm-inspection/map-of-realm-key-realm r))
                                            (realm-inspection/keyword? (realm-inspection/map-of-realm-key-realm r)))
                                        (realm/contains? @rec-var (realm-inspection/map-of-realm-value-realm r))))
                                 "map of json")

               (realm/restricted realm-inspection/enum
                                 ;; if values are json
                                 (fn [r]
                                   (every? @value-pred? (realm-inspection/enum-realm-values r)))
                                 "enum of json")

               (realm/restricted realm-inspection/tuple
                                 (fn [r]
                                   (every? #(realm/contains? @rec-var %) (realm-inspection/tuple-realm-realms r)))
                                 "tuples of json")

               (realm/restricted realm-inspection/intersection
                                 (fn [r]
                                   ;; an intersection of realms is a json realm, if any of the contained realms is a json realm
                                   (some #(realm/contains? @rec-var %) (realm-inspection/intersection-realm-realms r)))
                                 "intersection of json")
               (realm/restricted realm-inspection/union
                                 (fn [r]
                                   ;; a union of realms is a json realm, if all realms are a json realm
                                   (every? #(realm/contains? @rec-var %) (realm-inspection/union-realm-realms r)))
                                 "union of json")))

(def ^{:doc "The realm of json realms."} json-realm
  (json-realm-of (var json-realm) (var json?)))

(defn json-realm? [realm]
  (realm/contains? json-realm realm))

;; transit ******************************

(def transit-uuid #?(:clj realm/uuid
                     :cljs (realm/from-predicate "Transit uuid" transit/uuid?)))

(def ^{:doc "The realm of transit values"} transit
  (realm/union json
               transit-uuid
               (realm/set-of (realm/delay transit))
               (realm/map-of (realm/delay transit) (realm/delay transit))
               (realm/sequence-of (realm/delay transit))
               ;; TODO: decimals, bigint, dates and times? uris
               realm/symbol
               realm/keyword
               realm/char))

(defn transit? [v]
  (realm/contains? transit v))

(defn transit-uuid-realm? [v]
  #?(:clj (realm-inspection/uuid? v)
     :cljs (and (realm-inspection/from-predicate? v)
                (= transit/uuid? (realm-inspection/predicate v)))))

(defn ^:no-doc transit-realm-of [rec-var value-pred?]
  (realm/union (json-realm-of rec-var value-pred?)
               transit-uuid
               realm-inspection/symbol
               realm-inspection/keyword
               realm-inspection/char
               ;; realm-inspection/rational is not supported, I think.

               ;; map-with-keys and map-with-tag with any keys (unlike json, where keys have to be strings)
               (realm/restricted realm-inspection/map-with-keys
                                 (fn [r]
                                   (let [m (realm-inspection/map-with-keys-realm-map r)]
                                     (and (every? @value-pred? (keys m))
                                          (every? #(realm/contains? @rec-var %) (vals m)))))
                                 "map of transit")
               (realm/restricted realm-inspection/map-with-tag
                                 (fn [r]
                                   (let [tag (realm-inspection/map-with-tag-realm-key r)
                                         value (realm-inspection/map-with-tag-realm-value r)]
                                     (and (@value-pred? tag)
                                          (@value-pred? value))))
                                 "map with json tag")

               (realm/restricted realm-inspection/map-of
                                 (fn [r]
                                   (and (realm/contains? @rec-var (realm-inspection/map-of-realm-key-realm r))
                                        (realm/contains? @rec-var (realm-inspection/map-of-realm-value-realm r))))
                                 "map of transit")
               (realm/restricted realm-inspection/set-of
                                 (fn [r]
                                   (realm/contains? @rec-var (realm-inspection/set-of-realm-realm r)))
                                 "set of transit")
               (realm/restricted realm-inspection/intersection
                                 (fn [r]
                                   ;; an intersection of realms is a transit realm, if any of the contained realms is a transit realm
                                   (some #(realm/contains? @rec-var %) (realm-inspection/intersection-realm-realms r)))
                                 "intersection of transit")
               (realm/restricted realm-inspection/union
                                 (fn [r]
                                   ;; a union of realms is a transit realm, if all realms are a transit realm
                                   (every? #(realm/contains? @rec-var %) (realm-inspection/union-realm-realms r)))
                                 "union of transit")))

(def ^{:doc "The realm of transit realms."} transit-realm
  (transit-realm-of (var transit-realm) (var transit?)))

(defn transit-realm? [realm]
  (realm/contains? transit-realm realm))

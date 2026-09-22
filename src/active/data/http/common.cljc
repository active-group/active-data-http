(ns active.data.http.common
  (:require [active.data.translate.formatter :as formatter]
            [active.data.translate.translator :as translator]
            [active.data.http.realms :as realms]
            ;; [active.data.realm :as realm #?@(:cljs [:include-macros true])]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.translate.format :as format])
  #?(:clj (:import (java.time LocalDate OffsetDateTime OffsetTime Instant LocalTime LocalDateTime)
                   (java.time.format DateTimeParseException))))

#?(:clj
   ;; No cljs support for now, as js/Date is so messed up with local, zoned and utc handling.
   (def iso-date-formatter
     (formatter/simple
      (translator/translator (fn from-extern [s]
                               (when-not (realms/iso-date? s)
                                 (throw (translator/format-error "Not an ISO date" s)))
                               (try (LocalDate/parse s)
                                    (catch DateTimeParseException e
                                      (throw (translator/format-error (str "Invalid date (" (.getMessage e) ")") s)))))
                             (fn to-extern [v]
                               (assert (instance? LocalDate v) v)
                               (.toString ^LocalDate v))

                             realms/iso-date))))

#?(:clj
   (def iso-date-time-formatter
     ;; No cljs support for now, as js/Date is so messed up with local, zoned and utc handling.
     (formatter/simple
      (translator/translator (fn from-extern [s]
                               (when-not (realms/iso-date-time? s)
                                 (throw (translator/format-error "Not an ISO date-time" s)))
                               (try (LocalDateTime/parse s)
                                    (catch DateTimeParseException e
                                      (throw (translator/format-error (str "Invalid date-time (" (.getMessage e) ")") s)))))
                             (fn to-extern [v]
                               (assert (instance? LocalDateTime v) v)
                               (.toString v))
                             realms/iso-date-time))))

#?(:clj
   (def iso-time-formatter
     ;; No cljs support for now, as js/Date is so messed up with local, zoned and utc handling.
     (formatter/simple
      (translator/translator (fn from-extern [s]
                               (when-not (realms/iso-time? s)
                                 (throw (translator/format-error "Not an ISO time" s)))
                               (try (LocalTime/parse s)
                                    (catch DateTimeParseException e
                                      (throw (translator/format-error (str "Invalid time (" (.getMessage e) ")") s)))))
                             (fn to-extern [v]
                               (assert (instance? LocalTime v) v)
                               (.toString v))
                             realms/iso-time))))

#?(:clj
   (def iso-offset-date-time-formatter
     ;; No cljs support for now, as js/Date is so messed up with local, zoned and utc handling.
     (formatter/simple
      (translator/translator (fn from-extern [s]
                               (when-not (realms/iso-offset-date-time? s)
                                 (throw (translator/format-error "Not an ISO date-time" s)))
                               (try (OffsetDateTime/parse s)
                                    (catch DateTimeParseException e
                                      (throw (translator/format-error (str "Invalid date-time (" (.getMessage e) ")") s)))))
                             (fn to-extern [v]
                               (assert (instance? OffsetDateTime v) v)
                               (.toString v))
                             realms/iso-offset-date-time))))

#?(:clj
   (def iso-offset-time-formatter
     ;; No cljs support for now, as js/Date is so messed up with local, zoned and utc handling.
     (formatter/simple
      (translator/translator (fn from-extern [s]
                               (when-not (realms/iso-offset-time? s)
                                 (throw (translator/format-error "Not an ISO time" s)))
                               (try (OffsetTime/parse s)
                                    (catch DateTimeParseException e
                                      (throw (translator/format-error (str "Invalid time (" (.getMessage e) ")") s)))))
                             (fn to-extern [v]
                               (assert (instance? OffsetTime v) v)
                               (.toString v))
                             realms/iso-offset-time))))

(def iso-instant-formatter
  (formatter/simple
   (translator/translator (fn from-extern [s]
                            (when-not (realms/iso-instant? s)
                              (throw (translator/format-error "Not an ISO instant" s)))
                            #?(:clj (try (.toInstant (OffsetDateTime/parse s))
                                         (catch DateTimeParseException e
                                           (throw (translator/format-error (str "Invalid instant (" (.getMessage e) ")") s))))
                               :cljs (let [r (js/Date.parse s)]
                                       (if (js/isNaN r)
                                         (throw (translator/format-error "Invalid instant" s))
                                         (js/Date. r)))))
                          (fn to-extern [v]
                            #?(:clj (do (assert (instance? Instant v) v)
                                        (.toString v))
                               :cljs (do (assert (instance? js/Date v) v)
                                         (.toISOString v))))
                          realms/iso-instant)))

(def date-and-time-iso-string-formats
  (fn [realm]
    (cond
       ;; instants, date, and date-time to iso strings
      #?@(:clj [(realms/local-date-realm? realm) iso-date-formatter
                (realms/local-date-time-realm? realm) iso-date-time-formatter
                (realms/local-time-realm? realm) iso-time-formatter

                (realms/offset-date-time-realm? realm) iso-offset-date-time-formatter
                (realms/offset-time-realm? realm) iso-offset-time-formatter])

      (realms/instant-realm? realm) iso-instant-formatter

      :else nil)))

(def uuid-string-formatter
  (formatter/simple
   (translator/translator (fn from-extern [s]
                            (let [r (when (string? s)
                                      (parse-uuid s))]
                              (if (nil? r)
                                (throw (translator/format-error "Not a uuid string" s))
                                r)))
                          (fn to-extern [uuid]
                            (assert (uuid? uuid) uuid)
                            (str uuid))
                          realms/uuid-string)))

(defn- check-range [from to v]
  (when-not (and (or (nil? from)
                     (<= from v))
                 (or (nil? to)
                     (<= v to)))
    (throw (translator/format-error (str  (cond
                                            (nil? from) (str "Larger than" to)
                                            (nil? to) (str "Smaller than" from)
                                            :else
                                            (str "Not in range " "[" from ", " to "]"))) v))))

(defn- integer-string-formatter [from to]
  (let [ext
        ;; Note: losing range info here; but that would be hard as a pattern.
        (realms/string-pattern #"[-]?[0-9]+")]
    (formatter/simple
     #?(:cljs (translator/translator (fn [s]
                                       (let [r (js/parseInt s 10)]
                                         (if (js/isNaN r)
                                           (throw (translator/format-error "Not an integer" s))
                                           (do
                                             (check-range from to r)
                                             r))))
                                     (fn [i]
                                       (.toString i))
                                     ext)
        :clj (translator/translator (fn [s]
                                      (let [r (try (Integer/parseInt s)
                                                   (catch NumberFormatException _e
                                                     (throw (translator/format-error "Not an integer" s))))]
                                        (check-range from to r)
                                        r))
                                    (fn [i]
                                      (Integer/toString i))
                                    ext)))))

(def ^{:doc "Defines a default format for string coercions, used for path and query parameters.
  Only supports realms that have an 'obvious' string representation, and `nil` for optionals."}
  default-string-format
  (format/combine-formats
   date-and-time-iso-string-formats
   (fn [realm]
     (cond
       (or (realm-inspection/string? realm)
           (realm-inspection/optional? realm)
           ;; intersections are ok I think; if all inner realms have a formatter;
           ;; unions are not in general (everything is a string; we would have to guess/test what it is).
           (realm-inspection/intersection? realm))
       (formatter/identity realm)

       (realm-inspection/uuid? realm) uuid-string-formatter

       (realm-inspection/integer-from-to? realm) (integer-string-formatter (realm-inspection/integer-from-to-realm-from realm)
                                                                           (realm-inspection/integer-from-to-realm-to realm))

       (realm-inspection/enum? realm)
       (let [vals (realm-inspection/enum-realm-values realm)]
         (if (every? string? vals)
           (formatter/identity realm)
           ;; Note: we could try to support more here, like ints with a mapping to (str int), but that might go to far already;
           ;; as we "risk" having to guess what it is, like with an (enum 3 "3"). It should probably better be left to an explicit mapping.
           nil))

       :else nil))))

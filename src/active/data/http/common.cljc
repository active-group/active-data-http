(ns active.data.http.common
  (:require [active.data.translate.formatter :as formatter]
            [active.data.translate.translator :as translator]
            [active.data.http.realms :as realms]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]))

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
                                   realm/string)
      :clj (translator/translator (fn [s]
                                    (let [r (try (Integer/parseInt s)
                                                 (catch NumberFormatException _e
                                                   (throw (translator/format-error "Not an integer" s))))]
                                      (check-range from to r)
                                      r))
                                  (fn [i]
                                    (Integer/toString i))
                                  ;; Note: losing range info here; but that would be hard as a pattern.
                                  ;; TODO realms/pattern-string "[-]?[0-9]+" or so?
                                  realm/string))))

(defn- stringable? [v]
  (or (string? v)
      (int? v)))

(def ^{:doc "Defines a default format for string coercions, used for path and query parameters.
  Only supports realms that have an 'obvious' string representation, and `nil` for optionals."}
  default-string-format
  (fn [realm]
    (cond
      (or (realm-inspection/string? realm)
          (realm-inspection/optional? realm)
          ;; intersection and union are ok, if the base realms are supported
          (realm-inspection/intersection? realm)
          (realm-inspection/union? realm))
      (formatter/identity realm)

      (realm-inspection/uuid? realm) uuid-string-formatter

      (realm-inspection/integer-from-to? realm) (integer-string-formatter (realm-inspection/integer-from-to-realm-from realm)
                                                                          (realm-inspection/integer-from-to-realm-to realm))

      (realm-inspection/enum? realm)
      (let [vals (realm-inspection/enum-realm-values realm)]
        (if (every? string? vals)
          (formatter/identity realm)
          (if (every? stringable? vals)
            (formatter/constants (into {}
                                       (map (fn [v]
                                              [v (str v)])
                                            vals)))
            nil)))

      :else nil)))

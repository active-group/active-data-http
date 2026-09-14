(ns active.data.http.common
  (:require [active.data.translate.formatter :as formatter]
            [active.data.translate.translator :as translator]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]))

(def ^:private uuid-translator
  (translator/translator (fn [s]
                           (if (string? s)
                             (or (parse-uuid s) s)
                             (throw (translator/format-error "Not a uuid string" s))))
                         (fn [uuid]
                           (assert (uuid? uuid) uuid)
                           (str uuid))
                         realm/string))

(defn- integer-translator [realm]
  #?(:cljs (translator/translator (fn [s]
                                    (let [r (js/parseInt s 10)]
                                      (if (js/isNaN r)
                                        (throw (translator/format-error "Not an integer" s))
                                        (do
                                          (when-not (realm/contains? realm r)
                                            (throw (translator/format-error "Integer out of range" r)))
                                          r))))
                                  (fn [i]
                                    (.toString i))
                                  realm/string)
     :clj (translator/translator (fn [s]
                                   (let [r (try (Integer/parseInt s)
                                                (catch NumberFormatException _e
                                                  (throw (translator/format-error "Not an integer" s))))]
                                     (when-not (realm/contains? realm r)
                                       (throw (translator/format-error "Integer out of range" r)))
                                     r))
                                 (fn [i]
                                   (Integer/toString i))
                                 realm/string)))

(def ^:private string-translator
  (translator/translator (fn [s]
                           (if (string? s)
                             s
                             (throw (translator/format-error "Not a string" s))))
                         identity
                         realm/string))

(defn- optional-formatter [realm]
  (fn [resolve]
    (let [t (resolve realm)]
      (translator/translator (fn [v]
                               (if (some? v)
                                 ((translator/from-extern t) v)
                                 nil))
                             (fn [v]
                               (if (some? v)
                                 ((translator/to-extern t) v)
                                 nil))
                             (realm/optional realm/string)))))

(defn- checked-enum [constants]
  (translator/translator (fn [v]
                           (if (contains? constants v)
                             v
                             (throw (translator/format-error (str "Not in set " (pr-str constants)) v))))
                         identity
                         (apply realm/enum constants)))

(def ^{:doc "Defines a default format for string coercions, used for path and query parameters.
  Only supports realms that have an 'obvious' string representation, and `nil` for optionals."}
  default-string-format
  (fn [realm]
    (cond
      (realm-inspection/string? realm) (formatter/simple string-translator)

      (realm-inspection/uuid? realm) (formatter/simple uuid-translator)

      (realm-inspection/optional? realm)
      (optional-formatter (realm-inspection/optional-realm-realm realm))

      (realm-inspection/integer-from-to? realm) (formatter/simple (integer-translator realm))

      (realm-inspection/enum? realm)
      (let [vals (realm-inspection/enum-realm-values realm)]
        (if (every? string? vals) ;; TODO: or the other things that have representations? int and uuid?
          (formatter/simple (checked-enum (realm-inspection/enum-realm-values realm)))
          nil))

      ;; intersection and union are ok, if the base realms are supported
      (realm-inspection/intersection? realm) (formatter/identity realm)
      (realm-inspection/union? realm) (formatter/identity realm)

      :else nil)))

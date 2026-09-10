(ns active.data.http.common
  (:require [active.data.translate.format :as format]
            [active.data.translate.formatter :as formatter]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.clojure.lens :as lens]))

(def ^:private uuid-lens
  (lens/xmap (fn [s]
               (if (string? s)
                 (or (parse-uuid s) s)
                 (throw (format/format-error "Not a uuid string" s))))
             (fn [uuid]
               (assert (uuid? uuid) uuid)
               (str uuid))))

(def ^:private integer-lens
  #?(:cljs (lens/xmap (fn [s]
                        (let [r (js/parseInt s 10)]
                          (if (js/isNaN r)
                            (throw (format/format-error "Not an integer" s))
                            r)))
                      (fn [i]
                        (.toString i)))
     :clj (lens/xmap (fn [s]
                       (try (Integer/parseInt s)
                            (catch NumberFormatException _e
                              (throw (format/format-error "Not an integer" s)))))
                     (fn [i]
                       (Integer/toString i)))))

(def ^:private string-lens
  (lens/xmap (fn [s]
               (if (string? s)
                 s
                 (throw (format/format-error "Not a string" s))))
             identity))

(defn- optional-lens [other]
  (lens/xmap (fn [v]
               (if (some? v)
                 (other v)
                 nil))
             (fn [v]
               (if (some? v)
                 (other nil v)
                 nil))))

(defn- optional-formatter [realm]
  (fn [resolve]
    (optional-lens (resolve realm))))

(def ^{:doc "Defines a default format for string coercions, used for path and query parameters. Only supports realm that have an 'obvious' string representation, and `nil` for optionals."}
  default-string-format
  (format/format ::default-string-format
                 (let [m {realm/string (formatter/simple string-lens)
                          realm/uuid (formatter/simple uuid-lens)
                          realm/integer (formatter/simple integer-lens)}]
                   (fn [realm]
                     (cond
                       (realm-inspection/optional? realm)
                       (optional-formatter (realm-inspection/optional-realm-realm realm))

                       ;; TODO: maybe we can support a bit more, and unions, enums. But not everything can be supported (not as much as for bodies)

                       ;; ranged integer?
                       ;; named delay?
                       ;; restricted, if we support the base realm?

                       #_#_(realm-inspection/enum? realm)
                         (let [vals (realm-inspection/enum-realm-values realm)]
                           (if (every? string? vals) ;; TODO: or the other things that have representations?
                             ))
                       ;; (realm-inspection/union? realm) of the realms we support otherwise

                       :else
                       (m realm))))))

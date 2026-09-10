(ns active.data.http.common
  (:require [active.data.translate.format :as format]
            [active.data.translate.formatter :as formatter]
            [active.data.realm :as realm]
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

(def ^{:doc "Defines a default format for string coercions, used for path and query parameters."}
  default-string-format
  (format/format ::default-string-format
                 ;; TODO: maybe we can support a bit more, and unions, enums. But not everything can be supported (not as much as for bodies)
                 {realm/string (formatter/simple lens/id)
                  realm/uuid (formatter/simple uuid-lens)
                  realm/integer (formatter/simple integer-lens)}))

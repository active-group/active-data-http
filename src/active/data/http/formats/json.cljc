(ns active.data.http.formats.json
  (:require [active.data.translate.format :as format]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.http.common :as common]
            [active.data.translate.formatter :as formatter]
            [active.data.translate.translator :as translator]
            [active.data.http.realms :as realms]))

;; Note: this is almost like json-realm without the restrictions
(def ^:private json-realm-plain (realms/json-realm-of (var realm/any) (var realms/json?)))

(def basic
  (format/combine-formats
   ;; some things have an 'obvious' coercion:
   common/date-and-time-iso-string-formats
   (fn [realm]
     (cond
       ;; uuid as a string
       (realm-inspection/uuid? realm) common/uuid-string-formatter

       ;; any (other) sequence as a vector
       (and (realm-inspection/sequence-of? realm)
            (not (realms/vector-of-realm? realm)))
       (fn [resolve]
         (let [it (resolve (realm-inspection/sequence-of-realm-realm realm))]
           (translator/translator (fn from-extern [v]
                                    (if (vector? v)
                                      (mapv (translator/from-extern it) v)
                                      (throw (translator/format-error "not a vector" v))))
                                  (fn to-extern [v]
                                    (mapv (translator/to-extern it) v))
                                  (realms/vector-of (translator/external-realm it)))))

       :else nil))
   ;; '1:1' json
   (fn [realm]
     ;; Note: checking for 'json-realm' would be too restrictive
     ;; here, because a tuple of some realms that supported by
     ;; formatters added later (like records) must be transformed here
     ;; as well. 'json-realm?' should hold for external-realm though.
     (cond
       (realm/contains? json-realm-plain realm) (formatter/identity realm)
       :else nil))))


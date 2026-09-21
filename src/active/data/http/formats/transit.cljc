(ns active.data.http.formats.transit
  (:require [active.data.translate.format :as format]
            [active.data.translate.formatter :as formatter]
            [active.data.translate.translator :as translator]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.http.formats.json :as json]
            #?(:cljs [cognitect.transit :as transit])
            [active.data.realm :as realm]
            [active.data.http.realms :as realms]))

;; Note: this is almost like transit-realm without the restrictions
(def ^:private transit-realm-plain (realms/transit-realm-of (var realm/any) (var realms/transit?)))

(def ^{:doc "A format supporting values that are directly compatible with transit+json."} basic
  (format/combine-formats
   (fn [realm]
     (cond
       (realm-inspection/uuid? realm)
       ;; Note: realm uuid is: clojure.core/uuid? (java.util.UUID) resp. cljs.core/uuid? (cljs.core/UUID)
       ;; transit uuid is: java.util.UUID  resp. com.cognitect.transit in cljs
       #?(:clj (formatter/identity realm)

          :cljs (formatter/simple
                 (translator/translator (fn from-extern [v]
                                          (when-not (transit/uuid? v)
                                            (throw (translator/format-error "Not a transit uuid" v)))
                                          (uuid (str v)))
                                        (fn to-extern [v]
                                          (transit/uuid (str v)))
                                        realms/transit-uuid)))

       :else nil))
   json/basic
   (fn [realm]
     ;; Note: transit-realm? would be too restrictive here.
     (if (realm/contains? transit-realm-plain realm)
       (formatter/identity realm)
       nil))))

(declare record-as-tuple)

(def
  ^{:doc "Translates values described by a realm to values usable by transit. The defaults cover most realms."} extended
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

  (format/combine-formats
   (fn [realm]
     (cond
       (realm-inspection/union? realm)
       ;; simply use the order/position of the sub-realm as a tag
       (formatter/tagged-union-tuple (->> (realm-inspection/union-realm-realms realm)
                                          (map-indexed vector)
                                          (into {})))

       (realm-inspection/record? realm)
       ;; Note: based on the tuple implementation
       (let [as-tuple (apply realm/tuple (map realm-inspection/record-realm-field-realm
                                              (realm-inspection/record-realm-fields realm)))]
         (fn [resolve]
           (record-as-tuple realm (resolve as-tuple))))

       :else nil))
   basic))

;; --------------------------------------------------------------------------------

(defn- record-as-tuple [record-realm tuple-translator]
  (let [getters (map realm-inspection/record-realm-field-getter
                     (realm-inspection/record-realm-fields record-realm))
        ctor (realm-inspection/record-realm-constructor record-realm)]
    (translator/translator (fn to-realm [v]
                             (apply ctor ((translator/from-extern tuple-translator) v)))
                           (fn from-realm [v]
                             ((translator/to-extern tuple-translator)
                              (mapv (fn [getter]
                                      (getter v))
                                    getters)))
                           (translator/external-realm tuple-translator))))


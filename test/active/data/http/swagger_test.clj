(ns active.data.http.swagger-test
  (:require [active.data.http.swagger :as swagger]
            [active.data.realm :as realm]
            [clojure.test :as t]))

(def my-parameters-description
  (swagger/make-parameters-description (realm/map-with-keys {:name realm/string
                                                             :age (realm/integer-from 0)})
                                       (realm/map-with-keys {:id realm/uuid})
                                       nil))  ; no header params

(def my-responses
  {200 {:body (realm/map-with-keys {:id realm/uuid
                                    :name realm/string
                                    :age (realm/integer-from 0)})}
   404 {:body (realm/enum "not found")}})

(def my-swagger-description
  (swagger/make-swagger-description my-parameters-description
                                    my-responses))

(swagger/swagger-spec my-swagger-description)

;; {:parameters
;;  ({:in :body,
;;    :name "body",
;;    :description "",
;;    :required true,
;;    :schema
;;    {:type "object",
;;     :properties {:name {:type "string"}, :age {:type "integer", :minimum 0}},
;;     :required [:name :age],
;;     :closed true}}
;;   {:in :query,
;;    :name :id,
;;    :description "",
;;    :required true,
;;    :type "string",
;;    :format "uuid"}),
;;  :definitions {},
;;  :responses
;;  {200
;;   {:schema
;;    {:type "object",
;;     :properties
;;     {:id {:type "string", :format "uuid"},
;;      :name {:type "string"},
;;      :age {:type "integer", :minimum 0}},
;;     :required [:id :name :age],
;;     :closed true}},
;;   404 {:schema {:enum ["not found"]}}}}

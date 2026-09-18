(ns active.data.http.swagger
  "Utilities for translating a active-data realm into a map that the reitit
  swagger utilities can use to generate apidocs.

  It lets you describe a swagger description as data, using realms, resulting in
  a [[swagger-description]]. You can then translate the [[swagger-description]]
  into a a swagger spec via [[swagger-spec]].

  # Example

  ```
  (require '[active.data.realm :as realm])
  (require '[active.data.http.swagger :as swagger])
  
  (def my-parameters-description 
    (swagger/make-parameters-description (realm/map-with-keys {:name realm/string
                                                               :age (realm/integer-from 0)})
                                         (realm/map-with-keys {:id realm/uuid})
                                         nil))  ; no header params

  (def my-responses
    {200 {:body (realm/map-with-keys {:id realm/uuid
                                      :name realm/string
                                      :age (realm/integer-from 0)})}
     404 {:body (realm/enum \"not found\")}})

  (def my-swagger-description
    (swagger/make-swagger-description my-parameters-description
                                      my-responses))

  (swagger/swagger-spec my-swagger-description)  ;; returns the realized swagger descrpition.
  ```  
  "
  (:require
   [active.data.http.json-schema :as json-schema]
   [active.data.realm.inspection :as realm-inspection]))

;; TODO: At the very minimum, these tasks are left.
;; - [ ] No definitions at all
;; - [ ] No (explicit?) resolution of recursive/delayed realms.
;; - [ ] Very little error checking/handling.
;; - [ ] No actual swagger-specific types.

(defn compile-body 
  "Break up the `parameters-realm` of a body parameters definiton into a map and
  wrap it in a sequence. One parameter realm corresponds to
  one output swagger-parameter."
  [parameters-realm]
  (let [swagger-schema (json-schema/json-schema-from-realm parameters-realm)]
    [{:in :body
      :name "body"
      :description ""     ; TODO: Maybe allow metadata on realm to display here?
      :required (not (realm-inspection/optional? parameters-realm))
      :schema swagger-schema}]))

(defn make-compile
  "Takes either `:query` or `:header` and returns a function that breaks up the
  `parameters-realm` of a query or header parameters definition into a sequence
  of swagger-parameters (maps). Each entry in the `parameters-realm` corresponds to
  one swagger-parameter."
  [in]  ; One of #{:header :query}.
  (fn [parameters-realm]
    (when parameters-realm
      ;; NOTE: call to json-schema-from-realm MUST return an "object" (with
      ;; `:properties` and `:required`).
      (let [{:keys [properties required]} (json-schema/json-schema-from-realm parameters-realm)]
        (->> properties
             (map (fn [[k {:keys [type] :as schema}]]
                    (merge {:in in
                            :name k
                            :description ""
                            ;; TODO: Does it even make sense for query params to be optional?
                            :required (contains? (set required) k)
                            :type type}
                           (dissoc schema :type)))))))))

(def compile-query "See [[make-compile-description]]." (make-compile :query))
(def compile-header "See [[make-compile-description]]." (make-compile :header))

(defn- compile-parameters [body-realm query-realm header-realm]
  (concat (when body-realm (compile-body body-realm))
          (when query-realm (compile-query query-realm))
          (when header-realm (compile-header header-realm))))

(defn- compile-responses [responses]
  (->> (for [[status response-realm] responses]
         [status {:schema (json-schema/json-schema-from-realm (:body response-realm))}])
       (into {})))

(defn swagger-spec
  "Return a swagger spec map from body, query, header, and responses."
  [body-realm query-realm header-realm responses-realms]
  {:parameters (compile-parameters body-realm query-realm header-realm)
   :definitions {}
   :responses (compile-responses responses-realms)})


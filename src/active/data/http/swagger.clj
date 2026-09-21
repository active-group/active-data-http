(ns active.data.http.swagger
  "Utilities for translating a active-data realm into a map that the reitit
  swagger utilities can use to generate apidocs.

  It lets you describe a swagger description as data, using
  realms. You can then translate it into a a swagger spec via
  [[swagger-spec]].

  # Example

  ```
  (require '[active.data.realm :as realm])
  (require '[active.data.http.swagger :as swagger])
  
  (def my-responses
    {200 {:body (realm/map-with-keys {:id realm/uuid
                                      :name realm/string
                                      :age (realm/integer-from 0)})}
     404 {:body (realm/enum \"not found\")}})

  (swagger/swagger-spec {:name realm/string
                         :age (realm/integer-from 0)}
                        {:id realm/uuid}
                        nil
                        my-responses)
  ```  
  "
  (:require
   [active.data.http.json-schema :as json-schema]
   [active.data.realm.inspection :as realm-inspection]))

(defn- get-description [realm]
  ;; TODO: Maybe allow metadata on realm to display here?
  ;; should be a human-readable one, so realm/description is not really the right thing here.
  "")

(defn- de-optional [realm]
  (if (realm-inspection/optional? realm)
    (realm-inspection/optional-realm-realm realm)
    realm))

(defn- compile-body
  "Break up the `parameters-realm` of a body parameters definiton into a map and
  wrap it in a sequence. One parameter realm corresponds to
  one output swagger-parameter."
  [realm]
  (let [swagger-schema (json-schema/json-schema-from-realm (de-optional realm))]
    [{:in :body
      :name "body"
      :description (get-description realm)
      :required (not (realm-inspection/optional? realm))
      :schema swagger-schema}]))

(defn- make-compile-map
  "Takes either `:query` or `:header` and returns a function that breaks up the
  `parameters-realm` of a query or header parameters definition into a sequence
  of swagger-parameters (maps). Each entry in the `parameters-realm` corresponds to
  one swagger-parameter."
  [in] ;; One of #{:header :query}.
  (fn [parameters-realm-map]
    (->> parameters-realm-map
         (mapv (fn [[k realm]]
                 (let [schema (json-schema/json-schema-from-realm (de-optional realm))]
                   (merge {:in in
                           :name k
                           :description (get-description realm)
                           :required (not (realm-inspection/optional? realm))}
                          schema)))))))

(def ^:private compile-query "See [[make-compile-description]]." (make-compile-map :query))
(def ^:private compile-header "See [[make-compile-description]]." (make-compile-map :header))

(defn- compile-parameters [body query header]
  (vec (concat (when body (compile-body body))
               (when query (compile-query query))
               (when header (compile-header header)))))

(defn- compile-responses [responses]
  (->> (for [[status response-realm] responses]
         [status {:schema (json-schema/json-schema-from-realm (:body response-realm))}])
       (into {})))

(defn swagger-spec
  "Return a swagger spec map from body, query, header, and responses.
  `body-realm` must be a realm or nil.
  `query-realms` must be a map of keywords to realms.
  `header-realms` must be a map of lower-case strings to realms.
  `response-realms` must be a map of status code integers to a map {:body <realm>}.
  "
  [body-realm query-realms header-realms response-realms]
  (assert (or (nil? body-realm)
              (realm-inspection/realm? body-realm))
          body-realm)
  (assert (or (nil? query-realms)
              (and (map? query-realms)
                   (every? keyword? (keys query-realms))
                   (every? realm-inspection/realm? (vals query-realms))))
          query-realms)
  (assert (or (nil? header-realms)
              (and (map? header-realms)
                   (every? string? (keys header-realms))
                   (every? realm-inspection/realm? (vals header-realms))))
          header-realms)
  (assert (or (nil? response-realms)
              (and (map? response-realms)
                   (every? integer? (keys response-realms))
                   (every? realm-inspection/realm? (map :body (vals response-realms)))))
          response-realms)

  ;; TODO: maybe use 'named realms' as 'definition points'? Although different realms may have the same name.
  {:parameters (compile-parameters body-realm query-realms header-realms)
   :definitions {}
   :responses (compile-responses response-realms)})

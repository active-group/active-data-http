(ns active.data.http.swagger-test
  (:require [active.data.http.swagger :as swagger]
            [active.data.realm :as realm]
            [active.data.http.realms :as realms]
            [clojure.test :as t]))

(t/deftest swagger-spec-test
  (t/is (= {:parameters [{:in "body", :name "body", :description "", :required true,
                          :schema {:type "object", :properties {:name {:type "string", :minLength 1, :maxLength 50},
                                                                :age {:type "integer", :minimum 0, :maximum 1000}},
                                   :required [:name :age], :closed false}}
                         {:in "path", :name :a, :description "", :required true, :type "string"}
                         {:in "query", :name :id, :description "", :required true, :type "string", :format "uuid"}
                         {:in "header", :name "x-header", :description "", :required true, :type "string"}],
            :definitions {},
            :responses {200 {:schema {:type "object", :properties {:id {:type "string", :format "uuid"},
                                                                   :size {:type "integer", :minimum 0}},
                                      :required [:id :size], :closed false}},
                        404 {:schema {:enum ["not found"]}}}}

           (swagger/swagger-spec (realm/map-with-keys {:name (realms/string-of-length 1 50)
                                                       :age (realm/integer-from-to 0 1000)})
                                 {:a realm/string}
                                 {:id realms/uuid-string}
                                 {"x-header" realm/string}
                                 {200 {:body (realm/map-with-keys {:id realms/uuid-string
                                                                   :size (realm/integer-from 0)})}
                                  404 {:body (realm/enum "not found")}}))))


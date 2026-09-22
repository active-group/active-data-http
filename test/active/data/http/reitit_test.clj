(ns active.data.http.reitit-test
  (:require [active.data.http.reitit :as sut]
            [active.data.http.shared-example :as ex]
            [active.data.realm :as realm]
            [reitit.ring.coercion :as rrc]
            [reitit.ring :as ring]
            [reitit.openapi :as openapi]
            [reitit.swagger :as swagger]
            [clojure.test :as t]))

(def plus-endpoint
  {:coercion (sut/realm-coercion ex/my-body-format)
   :parameters {:body ex/plus-request
                :path {:bar realm/integer}
                :query {:foo realm/integer}
                :header {"my-header" (realm/optional realm/integer)}}
   :responses {200 {:body ex/plus-response}}
   :handler (fn [{:keys [parameters]}]
              #_(println "final parameters:" (pr-str parameters))
              (let [total (+ (-> parameters :body ex/req-x)
                             (-> parameters :body ex/req-y)
                             (-> parameters :path :bar)
                             (-> parameters :query :foo))]
                {:status 200
                 :body (ex/plus-response {ex/res-value total})}))})

(def app
  (ring/ring-handler
   (ring/router
    [["/api" ["/plus/:bar" {:name ::plus
                            :post plus-endpoint}]]
     ["/openapi" {:get {:handler (openapi/create-openapi-handler)
                        :openapi {:openapi "3.1.0"
                                  :info {:title "Foo"}}
                        :no-doc true}}]
     ["/swagger.json" {:get {:handler (swagger/create-swagger-handler)
                             :no-doc true}}]]

    {:data {:middleware [rrc/coerce-exceptions-middleware
                         rrc/coerce-request-middleware
                         rrc/coerce-response-middleware]}})
   #_(fn [req]
       {:status 404
        :body (str "URI: " (:uri req))})))

(t/deftest valid-request
  (t/is (= {:status 200, :body {:total 11}}
           (app {:request-method :post
                 :uri "/api/plus/3"
                 :query-params {"foo" "5"}
                 :body-params {:x 1 :y 2}})))

  (t/testing "extra query param is ok"
    ;; see open-model; this is what reitit expects per default, I think.
    (t/is (= {:status 200 :body {:total 11}}
             (app {:request-method :post
                   :uri "/api/plus/3"
                   :query-params {"foo" "5" "baz" "1"}
                   :body-params {:x 1 :y 2}})))))

(t/deftest invalid-request
  (t/testing "invalid body param, no :x nor :y"
    ;; TODO: Test the message? (not bad, but could be better)
    (t/is (= 400
             (:status (app {:request-method :post
                            :uri "/api/plus/3"
                            :query-params {"foo" "5"}
                            :body-params {:bla 2}})))))

  ;; should work if string-format wouldn't just return the original if integer parsing fails.
  (t/testing "missing query param, :foo is not set"
    ;; TODO: Test the message? (not bad, but could be better)
    (t/is (= 400
             (:status (app {:request-method :post
                            :uri "/api/plus/3"
                            :body-params {:x 1 :y 2}})))))

  ;; should work if string-format wouldn't just return the original if integer parsing fails.
  (t/testing "invalid path param; bla is not an integer"
    ;; TODO: Test the message? (not bad, but could be better)
    (t/is (= 400
             (:status (app {:request-method :post
                            :uri "/api/plus/bla"
                            :body-params {:x 1 :y 2}}))))))

(t/deftest openapi-test
  (t/is (= {:status 200,
            :body {:openapi "3.1.0", :x-id :some-id,
                   :info {:title "Foo"},
                   :paths {"/api/plus/{bar}"
                           {:post {:parameters [{:in "path", :name :bar, :required true, :schema {:type "string" :pattern "[-]?[0-9]+"}}
                                                {:in "query", :name :foo, :required true, :schema {:type "string" :pattern "[-]?[0-9]+"}}
                                                {:in "header", :name "my-header", :required false, :schema {:type "string" :pattern "[-]?[0-9]+"}}],
                                   :requestBody {:content {"application/json" {:schema {:type "object",
                                                                                        :properties {:x {:type "integer"}, :y {:type "integer"}},
                                                                                        :required [:x :y],
                                                                                        :closed false}}}},
                                   :responses {200 {:content {"application/json" {:schema {:type "object",
                                                                                           :properties {:total {:type "integer"}},
                                                                                           :required [:total],
                                                                                           :closed false}}}}}}}}}}
           (-> (app {:request-method :get
                     :uri "/openapi"})
               (assoc-in [:body :x-id] :some-id)))))

(t/deftest swagger-test
  (t/is (= {:status 200,
            :body {:swagger "2.0",
                   :x-id :some-id
                   ;; Note: swagger really has no schema for path parameters? I doubt it a bit.
                   :paths {"/api/plus/{bar}"
                           {:post {:parameters [{:in :body, :name "body", :description "", :required true,
                                                 :schema {:type "object", :properties {:x {:type "integer"},
                                                                                       :y {:type "integer"}},
                                                          :required [:x :y], :closed false}}
                                                {:in :query, :name :foo, :description "", :required true, :type "string" :pattern "[-]?[0-9]+"}
                                                {:in :header, :name "my-header", :description "", :required false, :type "string" :pattern "[-]?[0-9]+"}]
                                   :responses {200
                                               {:schema {:type "object", :properties {:total {:type "integer"}}, :required [:total], :closed false}}}}}},
                   :definitions {}}}
           (-> (app {:request-method :get
                     :uri "/swagger.json"})
               (assoc-in [:body :x-id] :some-id)))))

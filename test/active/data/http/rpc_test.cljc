(ns active.data.http.rpc-test
  (:require [active.data.http.rpc :as rpc #?@(:cljs [:include-macros true])]
            [active.data.record :as r #?@(:cljs [:include-macros true])]
            [clojure.test :as t #?@(:cljs [:include-macros true])]
            [active.data.realm :as realm]
            #?(:clj [reitit.ring :as ring])
            #?(:clj [reitit.core :as reitit.core])
            #?(:clj [active.data.http.rpc.reitit :as reitit])
            #?(:cljs [active.data.http.rpc.reacl-c :as reacl-c])
            #?(:cljs [reacl-c-basics.ajax :as reacl-c.ajax])))

(def my-api
  (-> (rpc/context "/internal-api")
      #?(:cljs (rpc/set-context-caller reacl-c/caller))))

(r/def-record dummy-int [d-x :- realm/integer])

(rpc/defn-rpc my-plus my-api :- dummy-int [x :- dummy-int, y :- realm/integer])

#?(:clj
   (do
     (def my-router
       (reitit/context-router
        my-api
        [(reitit/impl #'my-plus (fn [x y]
                                  (dummy-int d-x (+ (d-x x) y))))]))

     (def my-app
       (ring/ring-handler my-router))))

#?(:clj
   (t/deftest reitit-router-test
     (t/is (some? (reitit.core/match-by-path my-router "/internal-api/my-plus")))))

#?(:clj
   (t/deftest reitit-impls-test
     (t/is (= {:status 200, :body {:result [3]}}
              (my-app {:request-method :post
                       ;; :headers {"Accept" "application/transit+json"}
                       ;; :accept "application/transit+json"
                       :uri "/internal-api/my-plus"
                       :body-params {:args [[1] 2]}})))))

#?(:cljs
   (do
     (defn simulate-response [call response]
       (let [opts (reacl-c.ajax/request-options call)]
         ;; apply response interceptors (none are for the response)
         ;; apply :convert-response (map-ok-response fns)
         ((:convert-response opts) response)))

     (defn simulate-request [call]
       ;; apply request interceptors (will not include actual formatting of transit, i.e. into byte array)
       (let [opts (reacl-c.ajax/request-options call)]
         (reduce (fn [req interceptor]
                   ((:request interceptor) req))
                 (dissoc opts :convert-response :interceptors)
                 (:interceptors opts))))

     (t/deftest reacl-c-call-test
       (let [call (my-plus (dummy-int d-x 1) 2)]
         (t/is (= {:uri "/internal-api/my-plus"
                   :method "POST"
                   :format :transit
                   :response-format :transit
                   :params {:args [[1] 2]}}
                  (simulate-request call)))

         (t/is (= (dummy-int d-x 3)
                  (reacl-c.ajax/response-value (simulate-response call (reacl-c.ajax/make-response true {:result [3]})))))))))

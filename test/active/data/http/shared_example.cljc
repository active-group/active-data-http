(ns active.data.http.shared-example
  (:require [active.data.translate.format :as format]
            [active.data.translate.formatter :as formatter]
            [active.data.record :as r #?@(:cljs [:include-macros true])]
            [active.clojure.lens :as lens]
            [active.data.realm :as realm]))

(r/def-record plus-request
  [req-x :- realm/integer
   req-y :- realm/integer])

(r/def-record plus-response
  [res-value :- realm/integer])

(def my-body-format
  (format/format :my-body-format
                 {realm/integer (formatter/simple lens/id)
                  plus-request (formatter/record-map plus-request [:x :y])
                  plus-response (formatter/record-map plus-response [:total])}))


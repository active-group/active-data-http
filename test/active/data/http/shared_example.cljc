(ns active.data.http.shared-example
  (:require [active.data.translate.formatter :as formatter]
            [active.data.record :as r #?@(:cljs [:include-macros true])]
            [active.data.realm :as realm]))

(r/def-record plus-request
  [req-x :- realm/integer
   req-y :- realm/integer])

(r/def-record plus-response
  [res-value :- realm/integer])

(def my-body-format
  {realm/integer (formatter/identity realm/integer)
   plus-request (formatter/record-map plus-request [:x :y])
   plus-response (formatter/record-map plus-response [:total])})


(ns active.data.http.formats.transit-test
  (:require [active.data.http.formats.transit :as sut]
            [active.data.translate.core :as core]
            [active.data.realm :as realm]
            [active.data.record :as r #?@(:cljs [:include-macros true])]
            [clojure.test :as t #?@(:cljs [:include-macros true])]
            [active.data.translate.format :as format]))

;; TODO: clojurescript. and interop between clj and cljs (via transit values)

(defn translate-to [realm format value]
  ((core/translator-to realm format) value))

(defn translate-from [realm format value]
  ((core/translator-from realm format) value))

(defn roundtrip [realm v]
  (translate-to realm sut/transit-format
                (translate-from realm sut/transit-format v)))

(t/deftest empty-map-test
  (let [realm (realm/map-of realm/integer realm/integer)
        v {}
        t {}]
    (t/is (= t (translate-from realm sut/transit-format v)))
    (t/is (= v (translate-to realm sut/transit-format t)))))

(t/deftest void-result-test
  (let [realm (realm/map-with-keys {:result (realm/enum nil)})
        v {:result nil}
        t {:result nil}]
    (t/is (= t (translate-from realm sut/transit-format v)))
    (t/is (= v (translate-to realm sut/transit-format t)))))

(t/deftest tuple-test
  (let [realm (realm/tuple realm/string realm/integer)
        v ["foo" 42]
        t ["foo" 42]]
    (t/is (vector? (translate-from realm sut/transit-format v)))
    (t/is (= t (translate-from realm sut/transit-format v)))

    (t/is (vector? (translate-to realm sut/transit-format t)))
    (t/is (= v (translate-to realm sut/transit-format t)))

    (t/is (format/format-error? (try (translate-to realm sut/transit-format :foo)
                                     (catch #?(:clj Exception :cljs :default) e e))))))

(t/deftest empty-tuple-test
  (t/is (= [] (translate-from (realm/tuple) sut/transit-format [])))
  (t/is (vector? (translate-from (realm/tuple) sut/transit-format []))))

(r/def-record rec-ab
  [rec-a :- realm/string
   rec-b :- realm/integer])

(t/deftest transit-roundtrip-test
  ;; TODO: generator/property based testing would be great?
  (t/testing "positives"
    (t/is (= 1.2 (roundtrip realm/number 1.2)))
    ;; (t/is (= \c (roundtrip realm/char \c)))
    (t/is (= :x (roundtrip realm/keyword :x)))
    (t/is (= 'x (roundtrip realm/symbol 'x)))
    (t/is (= "x" (roundtrip realm/string "x")))
    (t/is (= false (roundtrip realm/boolean false)))
    (t/is (= #uuid "034929d6-aa85-4e66-b88f-c5685fb70fa2" (roundtrip realm/uuid #uuid "034929d6-aa85-4e66-b88f-c5685fb70fa2")))
    (t/is (= 42 (roundtrip realm/integer 42)))
    (t/is (= 1.2 (roundtrip realm/real 1.2)))
    (let [[any-1 value-1] [realm/integer 42]
          [any-2 value-2] [realm/string "foo"]]
      (t/is (= [value-1] (roundtrip (realm/sequence-of any-1) [value-1])))
      (t/is (= #{value-1} (roundtrip (realm/set-of any-1) #{value-1})))
      (t/is (= [value-1 value-2] (roundtrip (realm/tuple any-1 any-2) [value-1 value-2])))
      (t/is (= {value-1 value-2} (roundtrip (realm/map-with-keys {value-1 any-2}) {value-1 value-2})))
      (t/is (= {value-1 value-2} (roundtrip (realm/map-of any-1 any-2) {value-1 value-2})))
      (t/is (= value-1 (roundtrip (realm/delay any-1) value-1)))
      (t/is (= value-1 (roundtrip (realm/named :name any-1) value-1)))
      (t/is (= nil (roundtrip (realm/optional any-1) nil)))
      (t/is (= value-1 (roundtrip (realm/optional any-1) value-1))))
    (t/is (= :foo
             (roundtrip (realm/enum :foo "bar") :foo)))
    (t/is (= (rec-ab rec-a "foo" rec-b 42)
             (roundtrip rec-ab (rec-ab rec-a "foo" rec-b 42))))))

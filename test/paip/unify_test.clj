(ns paip.unify-test
  (:require [clojure.test :refer :all]
            [paip.unify :refer :all]))

(deftest unify-test-1
  (testing "unify-1"
    (is (= {'?x '?y}
           (unify '(f ?x) '(f ?y))))))

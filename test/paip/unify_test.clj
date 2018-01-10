(ns paip.unify-test
  (:require [clojure.test :refer :all]
            [paip.unify :refer :all]))

(deftest unify-test-1
  (testing "unify-1"
    (is (= {'?x '?y}
           (unify '(f ?x) '(f ?y))))))

(deftest unify-test-2
  (testing "unify-2"
    (is (= {'?a '?x, '?x '?y, '?y 0}
           (unify '(?a + ?a = 0) '(?x + ?y = ?y))))))
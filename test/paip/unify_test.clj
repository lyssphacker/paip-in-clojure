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

(deftest unifier-test-1
  (testing "unifier-1"
    (is (= '((?a * 5 expt 2) + (4 * 5) + 3)
           (unifier '((?a * ?x expt 2) + (?b * ?x) + ?c)
                    '(?z + (4 * 5) + 3))))))
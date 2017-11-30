(ns paip.patmatch_test
  (:require [clojure.test :refer :all]
            [paip.patmatch :refer :all]))

(deftest pat-match-test-1
  (testing "pat-match-1"
    (is (= {'?y '(d), '?x '(b c)}
           (pat-match
             '(a (?* ?x) (?* ?y) ?x ?y)
             '(a b c d (b c) (d)))))))

(deftest pat-match-test-2
  (testing "pat-match-2"
    (is (= {'?x 3, '?op '+, '?y 4, '?z 7}
           (pat-match
             '(?x ?op ?y is ?z (?if (= (?op ?x ?y) ?z)))
             '(3 + 4 is 7))))))

(deftest pat-match-test-3
  (testing "pat-match-3"
    (is (= nil
           (pat-match
             '(?x ?op ?y (?if (?op ?x ?y)))
             '(3 > 4))))))
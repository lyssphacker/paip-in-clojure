(ns paip.patmatch-test
  (:require [clojure.test :refer :all]
            [paip.patmatch :refer :all]))

(deftest pat-match-test-1
  (testing "pat-match"
    (is (= {'?y '(d), '?x '(b c)}
           (pat-match
             '(a (?* ?x) (?* ?y) ?x ?y)
             '(a b c d (b c) (d)))))))

(deftest pat-match-test-2
  (testing "pat-match"
    (is (= nil
           (pat-match
             '(?x ?op ?y (?if (?op ?x ?y)))
             '(3 > 4))))))

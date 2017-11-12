(ns paip.patmatch-test
  (:require [clojure.test :refer :all]
            [paip.patmatch :refer :all]))

(deftest pat-match-test
  (testing "pat-match"
    (is (= {'?y '(d), '?x '(b c)}
           (pat-match
             '(a (?* ?x) (?* ?y) ?x ?y)
             '(a b c d (b c) (d)))))))

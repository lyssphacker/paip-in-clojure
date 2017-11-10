(ns paip.eliza1-test
  (:require [clojure.test :refer :all]
            [paip.eliza1 :refer (pat-match)]))

(deftest pat-match-test
  (testing "pat-match"
    (is (= {'?x '(1 2 a b)}
           (pat-match '((?* ?x) a b (?* ?x))
                      '(1 2 a b a b 1 2 a b))))))
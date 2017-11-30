(ns paip.simple_test
  (:require [clojure.test :refer :all]
            [paip.simple :refer :all]))

(deftest generate-all-count-test
  (testing "generate-all"
    (is (= 256 (count (generate-all :sentence))))))

(deftest combine-all-test
  (testing "combine-all"
    (is (= (combine-all '((a) (b)) '((1) (2)))
           '((a 1) (b 1) (a 2) (b 2))))))

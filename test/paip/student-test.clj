(ns paip.student-test
  (:require [clojure.test :refer :all]
            [paip.student :refer :all]))

(deftest one-unknown-test-1
  (testing "one-unknown-1"
    (is (= 'a
           (one-unknown
             (mkexp 'a '= 1))))))

(deftest one-unknown-test-2
  (testing "one-unknown-2"
    (is (= 'b
           (one-unknown
             (mkexp
               (mkexp 'b '= 3)
               '=
               1))))))

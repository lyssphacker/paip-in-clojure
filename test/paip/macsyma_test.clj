(ns paip.macsyma-test
  (:require [clojure.test :refer :all]
            [paip.macsyma :refer :all]))

(deftest infix->prefix-simple
  (testing "macsyma-simple"
    (is
      (= '(+ 2 3)
         (infix->prefix '(2 + 3))))))

(deftest simp-1
  (testing "simp-1"
    (is
      (= 137
         (simp '(5 * 20 + 30 + 7))))))

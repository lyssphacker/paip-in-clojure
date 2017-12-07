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

(deftest simp-2
  (testing "simp-2"
    (is
      (= 0
         (simp '(y / z * (5 * x - (4 + 1) * x)))))))

(deftest simp-3
  (testing "simp-3"
    (is
      (= 'x
         (simp '((4 - 3) * x + (y / y - 1) * z))))))

(deftest simp-4
  (testing "simp-4"
    (is
      (= '(720 * x)
         (simp '(2 * x * 3 * x * 4 * (1 / x) * 5 * 6))))))

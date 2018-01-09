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

(deftest simp-5
  (testing "simp-5"
    (is
      (= 1
         (simp '(sin (x + x) * sin (d x expt 2 / d x) +
                     cos (2 * x) * cos (x * d 2 * y / d y)))))))

(deftest simp-6
  (testing "simp-6"
    (is
      (= '(-1/2 * (cos (x expt 2)))
         (simp '(int x * sin(x expt 2) d x))))))

(deftest simp-7
  (testing "simp-7"
    (is
      (= '((3 * ((x expt 4) / 4)) - (1/3 * ((x expt -2) / -2)))
         (simp '(int ((3 * x expt 3) - 1 / (3 * x expt 3)) d x))))))

(deftest simp-8
  (testing "simp-8"
    (is
      (= '(-4/3 * (((x expt 3) + 2) expt -2))
         (simp '(int 8 * x expt 2 / (x expt 3 + 2) expt 3 d x))))))
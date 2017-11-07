(ns paip.gps-test
  (:require [clojure.test :refer :all]
            [paip.gps :refer :all]))

(deftest gps-test-1
  (testing "gps-1"
    (is (= '(start
              executing-look-up-number
              executing-telephone-shop
              executing-tell-shop-problem
              executing-give-shop-money
              executing-shop-installs-battery
              executing-drive-son-to-school)
           (gps ['son-at-home 'car-needs-battery
                 'have-money 'have-phone-book]
                ['son-at-school]
                converted-school-ops)))))

(deftest gps-test-2
  (testing "gps-2"
    (is (= '(start
              executing-drive-son-to-school)
           (gps ['son-at-home 'car-works]
                ['son-at-school]
                converted-school-ops)))))

(deftest gps-test-3
  (testing "gps-3"
    (is (= '()
           (gps ['son-at-home 'car-needs-battery
                 'have-money 'have-phone-book]
                ['have-money 'son-at-school]
                converted-school-ops)))))

(deftest gps-test-4
  (testing "gps-4"
    (is (= '()
           (gps ['son-at-home 'car-needs-battery
                 'have-money 'have-phone-book]
                ['son-at-school 'have-money]
                converted-school-ops)))))

(deftest gps-test-5
  (testing "gps-5"
    (is (= '()
           (gps ['son-at-home 'car-needs-battery 'have-money]
                ['son-at-school]
                converted-school-ops)))))

(deftest gps-test-6
  (testing "gps-6"
    (is (= '(start)
           (gps ['son-at-home]
                ['son-at-home]
                converted-school-ops)))))

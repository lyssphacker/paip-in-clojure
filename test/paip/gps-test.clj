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

(def ab-block-ops (map convert-op (make-block-ops '(a b))))

(deftest gps-test-7
  (testing "gps-7"
    (is (= '(start executing-move-a-from-table-to-b)
           (gps ['a-on-table 'b-on-table 'space-on-a 'space-on-b 'space-on-table]
                ['a-on-b 'b-on-table]
                ab-block-ops)))))

(deftest gps-test-8
  (testing "gps-8"
    (is (= '(start executing-move-a-from-b-to-table executing-move-b-from-table-to-a)
           (gps ['a-on-b 'b-on-table 'space-on-a 'space-on-table]
                ['b-on-a]
                ab-block-ops)))))

(def abc-block-ops (map convert-op (make-block-ops '(a b c))))

(deftest gps-test-9
  (testing "gps-9"
    (is (= '(start executing-move-a-from-b-to-table executing-move-b-from-c-to-a executing-move-c-from-table-to-b)
           (gps ['a-on-b 'b-on-c 'c-on-table 'space-on-a 'space-on-table]
                ['b-on-a 'c-on-b]
                abc-block-ops)))))

(deftest gps-test-10
  (testing "gps-10"
    (is (= '(start executing-move-a-from-b-to-table executing-move-b-from-c-to-a executing-move-c-from-table-to-b)
           (gps ['a-on-b 'b-on-c 'c-on-table 'space-on-a 'space-on-table]
                ['c-on-b 'b-on-a]
                abc-block-ops)))))


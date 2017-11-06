(ns paip.gps1-test
  (:require [clojure.test :refer :all]
            [paip.gps1 :refer :all]))

(deftest gps-test-1
  (testing "gps-1"
    (is (= 'solved (gps ['son-at-home 'car-needs-battery
                         'have-money 'have-phone-book]
                        ['son-at-school]
                        school-ops)))))

(deftest gps-test-2
  (testing "gps-2"
    (is (= nil (gps ['son-at-home 'car-needs-battery
                     'have-money]
                    ['son-at-school]
                    school-ops)))))



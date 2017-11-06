(ns paip.gps-test
  (:require [clojure.test :refer :all]
            [paip.gps :refer :all]))

(deftest gps-test-1
  (testing "gps-1"
    (is (= 'solved (gps ['son-at-home 'car-needs-battery
                         'have-money 'have-phone-book]
                        ['son-at-school]
                        converted-school-ops)))))

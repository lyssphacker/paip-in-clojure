(ns paip.gps1-test
  (:require [clojure.test :refer :all]
            [paip.gps1 :refer :all]))

(deftest gps-test
  (testing "gps"
    (is (= 'solved (gps #{'son-at-home 'car-needs-battery
                          'have-money 'have-phone-book}
                        #{'son-at-school}
                        school-ops)))))



(ns paip.gps-test
  (:require [clojure.test :refer :all]
            [paip.gps :refer :all]))

(deftest starts-with-1
  (testing
    (is (= true
           (starts-with '(executing son-at-school)
                        'executing)))))

(deftest starts-with-2
  (testing
    (is (= false
           (starts-with '(son-at-school)
                        'executing)))))

(ns paip.auxfns-test
  (:require [clojure.test :refer :all]
            [paip.auxfns :refer :all]))

(def test-ops
  #{{:action   'drive-son-to-school
     :preconds #{'son-at-home 'car-works}
     :add-set  #{'son-at-school}
     :del-set  #{'son-at-home}},
    {:action   'shop-installs-battery
     :preconds #{'car-needs-battery 'shop-knows-problem 'shop-has-money}
     :add-set  #{'car-works}},
    {:action   'tell-shop-problem
     :preconds #{'in-communication-with-shop}
     :add-set  #{'shop-knows-problem}},
    {:action   'telephone-shop
     :preconds #{'know-phone-number}
     :add-set  #{'in-communication-with-shop}},
    {:action   'look-up-number
     :preconds #{'have-phone-book}
     :add-set  #{'know-phone-number}},
    {:action   'give-shop-money
     :preconds #{'have-money}
     :add-set  #{'shop-has-money},
     :del-set  #{'have-money}}})

(deftest find-all-test
  (testing "find-all"
    (is (> (count (find-all 'son-at-school test-ops))
           0))))

(def op
  {:action   'drive-son-to-school
   :preconds #{'son-at-home 'car-works}
   :add-set  #{'son-at-school}
   :del-set  #{'son-at-home}})

(deftest test-appropriate?
  (testing "appropriate?"
    (is (= true ((appropriate? 'son-at-school) op)))))

(ns paip.auxfns-test
  (:require [clojure.test :refer :all]
            [paip.auxfns :refer :all]))

(def test-ops
  #{{:action   'drive-son-to-school
     :preconds ['son-at-home 'car-works]
     :add-vec  ['son-at-school]
     ::del-vec ['son-at-home]},
    {:action   'shop-installs-battery
     :preconds ['car-needs-battery 'shop-knows-problem 'shop-has-money]
     :add-vec  ['car-works]},
    {:action   'tell-shop-problem
     :preconds ['in-communication-with-shop]
     :add-vec  ['shop-knows-problem]},
    {:action   'telephone-shop
     :preconds ['know-phone-number]
     :add-vec  ['in-communication-with-shop]},
    {:action   'look-up-number
     :preconds ['have-phone-book]
     :add-vec  ['know-phone-number]},
    {:action   'give-shop-money
     :preconds ['have-money]
     :add-vec  ['shop-has-money]
     ::del-vec ['have-money]}})

(deftest find-all-test
  (testing "find-all"
    (is (> (count (find-all 'son-at-school test-ops))
           0))))

(def op
  {:action   'drive-son-to-school
   :preconds ['son-at-home 'car-works]
   :add-vec  ['son-at-school]
   :del-vec  ['son-at-home]})

(deftest appropriate?-test
  (testing "appropriate?"
    (is (= true ((appropriate? 'son-at-school) op)))))

(deftest in?-test
  (testing "in?"
    (is (= true (in? 'son-at-home ['son-at-home 1])))))

(deftest pat-match-test
  (testing "pat-match"
    (is (= {:true :true, '?X 'vacation}
           (pat-match '(i need a ?X) '(i need a vacation))))))
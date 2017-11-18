(ns paip.auxfns-test
  (:require [clojure.test :refer :all]
            [paip.auxfns :refer :all]
            [paip.gps :refer (action?)]
            [paip.patmatch :refer (expand-pat-match-abbrev)]))

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

(deftest pat-match-test-1
  (testing "pat-match-1"
    (is (= {'?X 'vacation}
           (pat-match '(i need a ?X) '(i need a vacation))))))

(deftest pat-match-test-2
  (testing "pat-match-2"
    (is (= nil
           (pat-match '(i need a ?X) '(i really need a vacation))))))

(deftest pat-match-test-3
  (testing "pat-match-3"
    (is (= {'?X '(2 + 2)}
           (pat-match '(?X is ?X) '((2 + 2) is (2 + 2)))))))

(deftest subseqn-test-1
  (testing "subseqn-1"
    (is (= '(2 3) (subseqn 1 3 '(1 2 3 4))))))

(deftest subseqn-test-2
  (testing "subseqn-2"
    (is (= '() (subseqn 0 0 '(1 2 3 4))))))

(deftest count-if-test-1
  (testing "count-if-1"
    (is (= 2
           (count-if
             action?
             '(executing-that start))))))

(deftest fmap-test
  (testing "fmap"
    (is (= (fmap
             {'(?x* |.|)     '?x
              '(?x* |.| ?y*) '(?x ?y)}
             expand-pat-match-abbrev)
           {'((?* ?x) |.|)         '?x
            '((?* ?x) |.| (?* ?y)) '(?x ?y)}))))
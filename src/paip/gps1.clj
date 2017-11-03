(ns ^{:doc "First version of GPS (General Problem Solver)"}
paip.gps1
  (:require [clojure.set :refer :all]))

(def school-ops
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

(defn appropriate?
  "An op is appropriate to a goal if it is in its add list."
  [goal]
  (fn [op]
    (contains? (:add-set op) goal)))

(declare apply-op)

(defn achieve
  [state ops]
  (fn [goal]
    (or (contains? state goal)
        (some (apply-op state ops)
              (filter (appropriate? goal) ops)))))

(defn apply-op
  [state ops]
  (fn [op]
    (with-local-vars [local-state state]
      (when (every? (achieve state ops) (:preconds op))
        (print (list 'executing (:action op)))
        (var-set local-state (difference local-state (:del-list op)))
        (var-set local-state (union local-state (:add-list op)))
        local-state))))

(defn gps
  [state goals ops]
  (if (every? (achieve state ops) goals) 'solved))




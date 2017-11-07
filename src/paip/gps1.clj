(ns ^{:doc "First version of GPS (General Problem Solver)"}
paip.gps1
  (:require [clojure.set :refer :all]
            [paip.auxfns :refer (appropriate? contains-val?)]))

(def school-ops
  #{{:action   'drive-son-to-school
     :preconds ['son-at-home 'car-works]
     :add-vec  ['son-at-school]
     :del-vec  ['son-at-home]},
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
     :del-vec  ['have-money]}})

(declare apply-op)

(defn achieve
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  [state ops]
  (fn [goal]
    (or (contains-val? state goal)
        (some (apply-op state ops)
              (filter (appropriate? goal) ops)))))

(defn apply-op
  "Print a message and update *state* if op is applicable."
  [state ops]
  (fn [op]
    (with-local-vars [local-state state]
      (when (every? (achieve state ops) (:preconds op))
        (println (list 'executing (:action op)))
        (var-set local-state (difference @local-state (:del-vec op)))
        (var-set local-state (union @local-state (:add-vec op)))
        local-state))))

(defn gps
  "General Problem Solver: achieve all goals using *ops*."
  [state goals ops]
  (if (every? (achieve state ops) goals) 'solved))




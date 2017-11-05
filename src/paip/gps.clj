(ns ^{:doc "Final version of GPS"}
paip.gps
  (:require [clojure.inspector :refer (atom?)]
            [paip.gps1 :refer (school-ops)]))

(defn starts-with
  "Is this a list whose first element is x?"
  [list x]
  (and (not (atom? list))
       (= (first list) x)))

(defn executing?
  "Is x of the form: (executing ...) ?"
  [x]
  (starts-with x 'executing))

(defn convert-op
  "Make op conform to the (EXECUTING op) convention."
  [op]
  (if (some executing? (:add-set op))
    op
    (update op :add-set
            (fn [add-set]
              (cons
                (list 'executing (:action op))
                add-set)))))

(def converted-school-ops
  (map convert-op school-ops))

(declare achieve-all)

(defn gps
  [state goals ops]
  (filter (complement atom?)
          (achieve-all (cons '(start) state)
                       goals
                       [])))





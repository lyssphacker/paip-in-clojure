(ns ^{:doc "Final version of GPS"}
paip.gps
  (:require [clojure.inspector :refer (atom?)]
            [paip.gps1 :refer (school-ops)]
            [paip.auxfns :refer (find-all contains-val?)]
            [clojure.string :refer (starts-with?)]
            [clojure.set :refer (subset?)]))

(defn executing?
  "Does x's name starts with 'executing'?"
  [x]
  (starts-with? x "executing"))

(defn convert-op
  "Make op conform to the (EXECUTING op) convention."
  [op]
  (if (some executing? (:add-set op))
    op
    (update op :add-set
            (fn [add-set]
              (conj
                add-set
                (symbol (str 'executing- (name (:action op)))))))))

(def converted-school-ops
  (map convert-op school-ops))

(declare apply-op)

(defn achieve
  [state goal goal-stack ops]
  (cond (contains? state goal) state
        (contains? goal-stack goal) '()
        :else (some
                (fn [op]
                  (apply-op state goal op goal-stack))
                (find-all goal ops))))

(defn achieve-all
  [state goals goal-stack ops]
  (with-local-vars [current-state state]
    (if (and (every?
               (fn [g]
                 (var-set current-state
                          (achieve state g goal-stack ops)))
               goals)
             (subset? goals current-state)))))

(defn gps
  [state goals ops]
  (filter (complement atom?)
          (achieve-all (cons '(start) state)
                       goals
                       []
                       ops)))





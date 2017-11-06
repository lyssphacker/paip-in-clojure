(ns ^{:doc "Final version of GPS"}
paip.gps
  (:require [clojure.inspector :refer (atom?)]
            [paip.gps1 :refer (school-ops)]
            [paip.auxfns :refer (find-all contains-val?)]
            [clojure.string :refer (starts-with?)]
            [clojure.set :refer (subset? union)]))

(defn executing?
  "Does x's name starts with 'executing'?"
  [x]
  (starts-with? x "executing"))

(defn convert-op
  "Make op conform to the executing-op convention."
  [op]
  (if (some executing? (:add-vec op))
    op
    (update op :add-vec
            (fn [add-vec]
              (cons
                (symbol (str 'executing- (name (:action op))))
                add-vec)))))

(def converted-school-ops
  (map convert-op school-ops))

(declare achieve-all)

(defn apply-op
  "Return a new, transformed state if op is applicable."
  [state goal op goal-stack ops]
  (let [state2 (achieve-all state (:preconds op)
                            (cons goal goal-stack) ops)]
    (when-not (empty? state2)
      (union (filter (complement
                       (fn [x]
                         (contains-val? x (:del-vec op))))
                     state2)
             (:add-vec op)))))

(defn achieve
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  [state goal goal-stack ops]
  (cond (contains-val? state goal) state
        (contains-val? goal-stack goal) '()
        :else (some
                (fn [op]
                  (apply-op state goal op goal-stack ops))
                (find-all goal ops))))

(defn achieve-all
  "Achieve each goal, and make sure they still hold at the end."
  [state goals goal-stack ops]
  (with-local-vars [current-state state]
    (if (and (every?
               (fn [g]
                 (var-set current-state
                          (achieve state g goal-stack ops)))
               goals)
             (subset? goals @current-state))
      @current-state)))

(defn gps
  "General Problem Solver: from state, achieve goals using ops."
  [state goals ops]
  (filter (complement atom?)
          (achieve-all (cons 'start state)
                       goals
                       []
                       ops)))





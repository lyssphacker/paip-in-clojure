(ns ^{:doc "Final version of GPS"}
paip.gps
  (:require [clojure.inspector :refer (atom?)]
            [paip.gps1 :refer (school-ops)]
            [paip.auxfns :refer (find-all in?)]
            [clojure.string :refer (starts-with?)]
            [clojure.set :refer (union)]))

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
                         (in? x (:del-vec op))))
                     state2)
             (:add-vec op)))))

(defn subset-in?
  [set1 set2]
  (and (<= (count set1) (count set2))
       (every? #(in? set2 %) set1)))

(defn achieve
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  [state goal goal-stack ops]
  (cond (in? state goal) state
        (in? goal-stack goal) '()
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
             (subset-in? goals @current-state))
      @current-state)))

(defn gps
  "General Problem Solver: from state, achieve goals using ops."
  [state goals ops]
  (filter (complement atom?)
          (achieve-all (cons 'start state)
                       goals
                       []
                       ops)))





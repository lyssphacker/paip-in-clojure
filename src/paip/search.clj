(ns ^{:doc "Search routines from section 6.4"}
paip.search
  (:require [paip.auxfns :refer (funcall fail)]))

(defn tree-search
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  [states goal? successors combiner]
  (cond
    (empty? states) fail
    (funcall goal? (first states)) (first states)
    :else (tree-search
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal? successors combiner)))

(defn depth-first-search
  "Search new states first until goal is reached."
  [start goal? successors]
  (tree-search (vector start) goal? successors concat))

(defn binary-tree
  [x]
  (vector (* 2 x) (+ 1 (* 2 x))))

(defn is [value] (fn [x] (= x value)))

(defn prepend
  "Prepend y to start of x"
  [x y]
  (concat y x))

(defn breath-first-search
  "Search old states first until goal is reached."
  [start goal? successors]
  (tree-search (vector start) goal? successors prepend))
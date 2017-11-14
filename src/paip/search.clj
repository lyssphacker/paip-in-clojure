(ns ^{:doc "Search routines from section 6.4"}
paip.search
  (:require [paip.auxfns :refer (funcall fail)]
            [clojure.math.numeric-tower :as math :refer (abs)]))

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

(defn finite-binary-tree
  "Return a successor function that generates a binary tree"
  [n]
  (fn [x]
    (filter
      (fn [child]
        (<= child n))
      (binary-tree x))))

(defn diff
  "Return the function that finds the difference from num."
  [num]
  (fn [x] (math/abs (- x num))))

(defn sorter
  "Return a combiner function that sorts according to cost-fn."
  [cost-fn]
  (fn [new old]
    (sort
      #(< (cost-fn %1) (cost-fn %2))
      (concat new old))))

(defn best-first-search
  "Search lowest cost states first until goal is reached."
  [start goal? successors cost-fn]
  (tree-search
    (vector start)
    goal?
    successors
    (sorter cost-fn)))

(defn price-is-right
  "Return a function that measures the difference from price,
  but gives a big penalty for going over price."
  [price]
  (fn [x]
    (if (> x price)
      Integer/MAX_VALUE
      (- price x))))
(ns ^{:doc "Search routines from section 6.4"}
paip.search
  (:require [paip.auxfns :refer (funcall fail subseqn)]
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

(defn beam-search
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  [start goal? successors cost-fn beam-width]
  (tree-search
    (vector start)
    goal?
    successors
    (fn
      [old new]
      (let [sorted
            (funcall (sorter cost-fn) old new)]
        (if (> beam-width (count sorted))
          sorted
          (subseqn 0 beam-width sorted))))))

(def cities
  [{:name 'Atlanta :long 84.23 :lat 33.45}
   {:name 'Los-Angeles :long 118.15 :lat 34.03}
   {:name 'Boston :long 71.05 :lat 42.21}
   {:name 'Memphis :long 90.03 :lat 35.09}
   {:name 'Chicago :long 87.37 :lat 41.50}
   {:name 'New-York :long 73.58 :lat 40.47}
   {:name 'Denver :long 105.00 :lat 39.45}
   {:name 'Denver :long 105.00 :lat 39.45}
   (Oklahoma-City 97.28 35.26)
   (Eugene 123.05 44.03) (Pittsburgh 79.57 40.27)
   (Flagstaff 111.41 35.13) (Quebec 71.11 46.49)
   (Grand-Jct 108.37 39.05) (Reno 119.49 39.30)
   (Houston 105.00 34.00) (San-Francisco 122.26 37.47)
   (Indianapolis 86.10 39.46) (Tampa 82.27 27.57)
   (Jacksonville 81.40 30.22) (Victoria 123.21 48.25)
   (Kansas-City 94.35 39.06) (Wilmington 77.57 34.14)])
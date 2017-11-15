(ns ^{:doc "Search routines from section 6.4"}
paip.search
  (:require [paip.auxfns :refer (funcall fail subseqn find-first)]
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
   {:name 'Oklahoma-City :long 97.28 :lat 35.26}
   {:name 'Eugene :long 123.05 :lat 44.03}
   {:name 'Pittsburgh :long 79.57 :lat 40.27}
   {:name 'Flagstaff :long 111.41 :lat 35.13}
   {:name 'Quebec :long 71.11 :lat 46.49}
   {:name 'Grand-Jct :long 108.37 :lat 39.05}
   {:name 'Reno :long 119.49 :lat 39.30}
   {:name 'Houston :long 105.00 :lat 34.00}
   {:name 'San-Francisco :long 122.26 :lat 37.47}
   {:name 'Indianapolis :long 86.10 :lat 39.46}
   {:name 'Tampa :long 82.27 :lat 27.57}
   {:name 'Jacksonville :long 81.40 :lat 30.22}
   {:name 'Victoria :long 123.21 :lat 48.25}
   {:name 'Kansas-City :long 94.35 :lat 39.06}
   {:name 'Wilmington :long 77.57 :lat 34.14}])

(declare air-distance)

(defn neighbors
  "Find all cities within 1000 kilometers."
  [city]
  (filter
    (fn [c]
      (and (not= c city)
           (< (air-distance c city) 1000.0)))
    cities))

(defn city
  "Find the city with this name."
  [name]
  (find-first
    #(= (:name %1) name)
    cities))

(defn trip
  "Search for a way from the start to dest."
  [start dest]
  (beam-search
    start (is dest) neighbors
    (fn [c]
      (air-distance c dest))
    1))

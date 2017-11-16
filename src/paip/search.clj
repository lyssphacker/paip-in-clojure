(ns ^{:doc "Search routines from section 6.4"}
paip.search
  (:require [paip.auxfns :refer (funcall fail subseqn
                                         find-first bigdec1
                                         <<)]
            [clojure.math.numeric-tower :as math :refer (abs expt)]
            [clojure.pprint :refer (cl-format)]))

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

(def earth-diameter
  "Diameter of planet earth in kilometers."
  12765.0)

(defn deg->radians
  "Convert degrees and minutes to radians."
  [deg]
  (* (+ (Math/floor deg)
        (* (rem deg 1)
           (/ 100 60)))
     Math/PI
     (/ 1 180)))

(defn xyz-coords
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  [city]
  (let [psi (deg->radians (:lat city))
        phi (deg->radians (:long city))]
    (list (* (Math/cos psi) (Math/cos phi))
          (* (Math/cos psi) (Math/sin phi))
          (Math/sin psi))))

(defn distance
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  [point1 point2]
  (Math/sqrt
    (reduce
      +
      (map
        (fn [a b]
          (expt (- a b) 2))
        point1 point2))))

(defn air-distance
  "The great circle distance between two cities."
  [city1 city2]
  (let
    [d (distance (xyz-coords city1) (xyz-coords city2))]
    (* earth-diameter (Math/asin (/ d 2)))))

(defn trip
  "Search for a way from the start to dest."
  [start dest]
  (beam-search
    start (is dest) neighbors
    (fn [c]
      (air-distance c dest))
    1))

(defn make-path
  [& {:keys [state previous cost-so-far total-cost]
      :or   {previous    []
             cost-so-far 0
             total-cost  0}}]
  {:state       state
   :previous    previous
   :cost-so-far cost-so-far
   :total-cost  total-cost})

(defn print-path
  [path]
  (let [cost (bigdec1 (:total-cost path))
        state (:state path)]
    (println
      (<< "<Path to #{state} cost #{cost}>"))))

(defn is
  "Returns a predicate that tests for a given value."
  [value & {:keys [key]
            :or   {key identity}}]
  (fn [path]
    (= value (funcall key path))))

(defn path-saver
  [successors cost-fn cost-left-fn]
  (fn [old-path]
    (let [old-state (:state old-path)]
      (map
        (fn [new-state]
          (let [old-cost
                (+ (:cost-so-far old-path)
                   (funcall cost-fn old-state new-state))]
            (make-path
              :state new-state
              :previous old-path
              :cost-so-far old-cost
              :total-cost (+ old-cost (funcall
                                        cost-left-fn
                                        new-state)))))
        (funcall successors old-state)))))

(defn trip
  "Search for the best path from the start to dest."
  ([start dest]
   (trip start dest 1))
  ([start dest beam-width]
   (beam-search
     (make-path :state start)
     (is dest :key (fn [path] (:state path)))
     (path-saver neighbors air-distance
                 (fn [c] (air-distance c dest)))
     (fn [path] (:total-cost path))
     beam-width)))

(defn map-path
  "Call fn on each state in the path, collecting results."
  [fn path]
  (if (empty? path)
    '()
    (cons
      (funcall fn (:state path))
      (map-path fn (:previous path)))))

(defn show-city-path
  "Show the length of a path, and the cities along it."
  [path]
  (cl-format
    true
    "#<Path ~,1f km: ~{~:(~a~)~^ - ~}>"
    (:total-cost path)
    (reverse
      (map-path
        (fn [city] (:name city))
        path))))

(defn iter-wide-search
  [start goal? successors cost-fn & {:keys [width max]
                                     :or   {width 1
                                            max   100}}]
  "Search, increasing beam width from width to max.
  Return the first solution found at any width."
  (when-not (> width max)
    (or
      (beam-search start goal? successors cost-fn width)
      (iter-wide-search start goal? successors cost-fn
                        :width (+ width 1)
                        :max max))))

(defn new-states
  "Generate successor states that have not been seen before."
  [states successors state= old-states]
  (filter
    (complement
      (fn [state]
        (or
          (drop-while
            #(state= state %)
            states)
          (drop-while
            #(state= state %)
            old-states))))
    (funcall successors (first states))))
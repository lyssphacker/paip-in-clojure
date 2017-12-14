(ns ^{:doc "Auxiliary functions used by all other programs"}
paip.auxfns
  (:require [clojure.inspector :refer (atom?)]
            [clojure.walk :refer (postwalk-replace)])
  (:import (java.math RoundingMode)))

(defn random-elt [choices]
  "Choose an element from a list at random."
  (nth choices (rand-int (count choices))))

(defn mappend
  "Append the results of calling fn on each element of list."
  [fn list]
  (apply concat (map fn list)))

(defn in?
  "true if coll contains elm"
  [elm coll]
  (some #(= elm %) coll))

(defn appropriate?
  "An op is appropriate to a goal if it is in its add list."
  [goal]
  (fn [op]
    (in? goal (:add-vec op))))

(defn find-all
  "Find all those elements of sequence that match item"
  [goal ops]
  (filter (appropriate? goal) ops))

(defn show-stacktrace
  []
  (clojure.stacktrace/print-stack-trace *e 30))

(defmacro << [^String string]
  (let [-re #"#\{(.*?)\}"
        fstr (clojure.string/replace string -re "%s")
        fargs (map #(read-string (second %)) (re-seq -re string))]
    `(format ~fstr ~@fargs)))

(defn cons?
  [x]
  (not (atom? x)))

(defn funcall [f & args]
  "Applies function f to arguments args."
  (apply f args))

(defn subseqn
  "Creates a sequence that is a copy of the subsequence of
  sequence bounded by start and end."
  [start end coll]
  (when (or (< end start) (< start 0) (< end 0)
            (> start (count coll)) (> end (count coll)))
    (throw (Exception. "Start and/or are not valid.")))
  (drop start (take end coll)))

(defn find-first
  "Finds all items in collection which satisfy f predicate."
  [f coll]
  (first (filter f coll)))

(defn round [s]
  (fn [n]
    (.setScale n s RoundingMode/HALF_EVEN)))

(defn bigdec2
  "Returns scaled to 2 decimal digits
  and rounded BigDecimal value of n."
  [n]
  ((round 2) (bigdec n)))

(defn bigdec1
  "Returns scaled to 1 decimal digit
  and rounded BigDecimal value of n."
  [n]
  ((round 1) (bigdec n)))

;;;; PATTERN MATCHING FACILITY

(defn variable?
  "Is x a variable (a symbol beginning with `?')?"
  [x]
  (and (symbol? x)
       (= (get (name x) 0) \?)))

(def fail nil)

(def no-bindings {:true :true})

(defn extend-bindings
  [var val bindings]
  (if (= bindings no-bindings)
    (assoc {} var val)
    (assoc bindings var val)))

(defn match-variable
  [variable input bindings]
  (cond
    (not (contains? bindings variable)) (extend-bindings variable input bindings)
    (= input (variable bindings)) bindings
    :else fail))

(defn pat-match
  ([pattern input]
   (pat-match pattern input no-bindings))
  ([pattern input bindings]
   (cond (= bindings fail) fail
         (variable? pattern) (match-variable pattern input bindings)
         (= pattern input) bindings
         (and
           (cons? pattern)
           (cons? input)) (pat-match (rest pattern)
                                     (rest input)
                                     (pat-match (first pattern)
                                                (first input)
                                                bindings))
         :else fail)))

;;; ==============================

(defn position
  "Finds the index of item in a collection.
  If start is specified, it finds index of item
  in a sub-collection starting from start."
  ([coll item]
   (position coll item 0))
  ([coll item start]
   (let [subcoll (drop start coll)
         index (.indexOf subcoll item)]
     (if (= index -1)
       index
       (+ start index)))))

(defn adjoin
  "Tests whether item is the same as an existing element of list.
  If the item is not an existing element, adjoin adds it to list
  (as if by cons) and returns the resulting list; otherwise,
  nothing is added and the original list is returned. "
  [coll x]
  (if (= -1 (position coll x))
    (cons x coll)
    coll))

(defn starts-with
  "Is x a list whose first element is x?"
  [lst x]
  (and (cons? lst) (= (first lst) x)))

(defn prepend
  "Prepend y to start of x"
  [x y]
  (concat y x))

(defn member
  "Tests membership of item in coll."
  [coll item & {:keys [key test]
                :or   {key  identity
                       test =}}]
  (not
    (nil?
      (some
        (fn [x] (test x item))
        (map key coll)))))

(defn find-item
  "Finds item in coll. If not found returns nil."
  [coll item & {:keys [key test]
                :or   {key  identity
                       test =}}]
  (let [result
        (filter
          (fn [x] (test x item))
          (map key coll))]
    (if (empty? result)
      nil
      (first result))))

(defn merge-seqs
  "Merges sequences. Sorts them by comp applied to
  elements whose values are extracted by key."
  [lst1 lst2 comp & {:keys [key]
                     :or   {key identity}}]
  (let [result (concat lst1 lst2)]
    (sort-by key comp result)))

(defn count-if
  "Count items in sequence which satisfy predicate."
  [predicate sequence]
  (count
    (filter
      predicate
      sequence)))

(defn fmap [m f]
  "Map function f over both keys and values of map m."
  (into {} (for [[k v] m] [(f k) (f v)])))

(defn fmap-keys [m f]
  "Map function f over keys of map m."
  (into {} (for [[k v] m] [(f k) v])))

(defn fmap-values [m f]
  "Map function f over values of map m."
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-if
  "Returns elements from coll which do not satisfy pred."
  [pred coll]
  (filter
    (complement pred)
    coll))

(defn subst
  "Substitute new for old in a form."
  [new old form]
  (postwalk-replace
    {old new}
    form))

(defn eql?
  [x y]
  (cond
    (and (coll? x) (coll? y))
    (and (empty? x) (empty? y))
    :else (= x y)))

(defn third
  [x]
  (nth x 2))

(defn length=1
  "Is x a list of length 1?"
  [x]
  (and (cons? x) (empty? (rest x))))

(defn find-first-index
  [f coll x]
  (let [el (find-first f coll)]
    (.indexOf coll el)))
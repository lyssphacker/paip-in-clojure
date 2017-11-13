(ns ^{:doc "Pattern matcher from section 6.2"}
paip.patmatch
  (:require [paip.auxfns :refer (variable? fail no-bindings
                                           match-variable cons?
                                           position)]
            [clojure.inspector :refer (atom?)]))

(def pattern-fn-map
  {'?is  'match-is,
   '?or  'match-or,
   '?and 'match-and,
   '?not 'match-not,
   '?*   'segment-match,
   '?+   'segment-match+,
   '??   'segment-match?,
   '?if  'match-if})

(defn match-fn
  "Get the matching function for x,
  if it is a symbol that has one."
  [x]
  (when
    (symbol? x)
    (let [func (x pattern-fn-map)]
      (if (not (nil? func))
        (resolve func)))))

(defn segment-pattern?
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  [pattern]
  (and (cons? pattern) (cons? (first pattern))
       (symbol? (first (first pattern)))
       (not (nil? (match-fn (first (first pattern)))))))

(defn single-pattern?
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  [pattern]
  (and (cons? pattern)
       (not (nil? (match-fn (first pattern))))))

(defn segment-matcher
  "Call the right function for this kind of segment pattern."
  [pattern input bindings]
  ((match-fn (first (first pattern)))
    pattern input bindings))

(declare single-matcher)

(defn pat-match
  "Match pattern against input in the context of the bindings"
  ([pattern input]
   (pat-match pattern input no-bindings))
  ([pattern input bindings]
   (cond (= bindings fail) fail
         (variable? pattern) (match-variable pattern input bindings)
         (= pattern input) bindings
         (segment-pattern? pattern) (segment-matcher pattern input bindings)
         (single-pattern? pattern) (single-matcher pattern input bindings)
         (and
           (cons? pattern)
           (cons? input)) (pat-match (rest pattern)
                                     (rest input)
                                     (pat-match (first pattern)
                                                (first input)
                                                bindings))
         :else fail)))

(defn match-is
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  [var-and-pred input bindings]
  (let [var (first var-and-pred)
        pred (second var-and-pred)
        new-bindings (pat-match var input bindings)]
    (if (or (= new-bindings fail)
            (not ((resolve pred) input)))
      fail
      new-bindings)))

(defn match-and
  "Succeed if all the patterns match the input."
  [patterns input bindings]
  (cond (= bindings fail) fail
        (empty? patterns) bindings
        :else (match-and (rest patterns)
                         input
                         (pat-match (first patterns)
                                    input
                                    bindings))))

(defn match-or
  "Succeed if any one of the patterns match the input."
  [patterns input bindings]
  (if (empty? patterns)
    fail
    (let [new-bindings (pat-match
                         (first patterns)
                         input
                         bindings)]
      (if (= new-bindings fail)
        (match-or (rest patterns)
                  input
                  bindings)
        new-bindings))))

(defn match-not
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  [patterns input bindings]
  (if (match-or patterns input bindings)
    fail
    bindings))

(defn first-match-post
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  [pat1 input start]
  (cond (and (atom? pat1) (not (variable? pat1)))
        (position input pat1 start)
        (<= start (count input)) start
        :else nil))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against input."
  ([pattern input bindings]
   (segment-match pattern input bindings 0))
  ([pattern input bindings start]
   (let [var (second (first pattern))
         pat (rest pattern)]
     (if (empty? pat)
       (match-variable var input bindings)
       ;; We assume that pat starts with a constant
       ;; In other words, a pattern can't have 2 consecutive vars
       (let [pos (first-match-post (first pat) input start)]
         (if (nil? pos)
           fail
           (let [b2 (pat-match
                      pat
                      (drop pos input)
                      (match-variable
                        var
                        (take pos input)
                        bindings))]
             ;; If this match failed, try another longer one
             ;; If it worked, check that the variables match
             (if (= b2 fail)
               (segment-match pattern input bindings (+ pos 1))
               b2))))))))

(defn segment-match+
  "Match one or more elements of input."
  [pattern input bindings]
  (segment-match pattern input bindings 1))

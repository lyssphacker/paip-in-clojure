(ns ^{:doc "Pattern matcher from section 6.2"}
paip.patmatch
  (:require [paip.auxfns :refer (fail no-bindings
                                      match-variable cons?
                                      position funcall eql?
                                      third)]
            [clojure.inspector :refer (atom?)]
            [clojure.walk :refer (postwalk-replace)]))

(def single-pattern-fn-map
  {'?is  'match-is,
   '?or  'match-or,
   '?and 'match-and,
   '?not 'match-not,
   '?if  'match-if})

(def segment-pattern-fn-map
  {'?* 'segment-match,
   '?+ 'segment-match+,
   '?? 'segment-match?})

(defn match-fn
  "Get the matching function for x,
  if it is a symbol that has one."
  [x map]
  (when
    (symbol? x)
    (let [func (x map)]
      (if (not (nil? func))
        (resolve func)))))

(defn segment-pattern?
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  [pattern]
  (and (cons? pattern) (cons? (first pattern))
       (symbol? (first (first pattern)))
       (not (nil? (match-fn
                    (first (first pattern))
                    segment-pattern-fn-map)))))

(defn single-pattern?
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  [pattern]
  (and (cons? pattern)
       (not (nil? (match-fn
                    (first pattern)
                    single-pattern-fn-map)))))

(defn segment-matcher
  "Call the right function for segment pattern."
  [pattern input bindings variable?]
  ((match-fn
     (first (first pattern))
     segment-pattern-fn-map)
    pattern input bindings variable?))

(defn single-matcher
  "Call the right function for single pattern."
  [pattern input bindings variable?]
  ((match-fn
     (first pattern)
     single-pattern-fn-map)
    pattern input bindings variable?))

(defn pat-match
  "Match pattern against input in the context of the bindings"
  ([pattern input variable?]
   (pat-match pattern input no-bindings variable?))
  ([pattern input bindings variable?]
   (cond (= bindings fail) fail
         (variable? pattern) (match-variable pattern input bindings)
         (eql? pattern input) bindings
         (segment-pattern? pattern) (segment-matcher pattern input bindings variable?)
         (single-pattern? pattern) (single-matcher pattern input bindings variable?)
         (and
           (cons? pattern)
           (cons? input)) (pat-match (rest pattern)
                                     (rest input)
                                     (pat-match (first pattern)
                                                (first input)
                                                bindings
                                                variable?)
                                     variable?)
         :else fail)))

(defn match-is
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  [var-and-pred input bindings variable?]
  (let [var (second var-and-pred)
        pred (third var-and-pred)
        new-bindings (pat-match var input bindings variable?)]
    (if (or (= new-bindings fail)
            (not ((resolve pred) input)))
      fail
      new-bindings)))

(defn match-and
  "Succeed if all the patterns match the input."
  [patterns input bindings variable?]
  (cond (= bindings fail) fail
        (empty? patterns) bindings
        :else (match-and (rest patterns)
                         input
                         (pat-match (first patterns)
                                    input
                                    bindings
                                    variable?)
                         variable?)))

(defn match-or
  "Succeed if any one of the patterns match the input."
  [patterns input bindings variable?]
  (if (empty? patterns)
    fail
    (let [new-bindings (pat-match
                         (first patterns)
                         input
                         bindings
                         variable?)]
      (if (= new-bindings fail)
        (match-or (rest patterns)
                  input
                  bindings
                  variable?)
        new-bindings))))

(defn match-not
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  [patterns input bindings variable?]
  (if (match-or patterns input bindings variable?)
    fail
    bindings))

(defn first-match-pos
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  [pat1 input start variable?]
  (cond (and (atom? pat1) (not (variable? pat1)))
        (position input pat1 start)
        (<= start (count input)) start
        :else -1))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against input."
  ([pattern input bindings variable?]
   (segment-match pattern input bindings 0 variable?))
  ([pattern input bindings start variable?]
   (let [var (second (first pattern))
         pat (rest pattern)]
     (if (empty? pat)
       (match-variable var input bindings)
       ;; We assume that pat starts with a constant
       ;; In other words, a pattern can't have 2 consecutive vars
       (let [pos (first-match-pos (first pat) input start variable?)]
         (if (= -1 pos)
           fail
           (let [b2 (pat-match
                      pat
                      (drop pos input)
                      (match-variable
                        var
                        (take pos input)
                        bindings)
                      variable?)]
             ;; If this match failed, try another longer one
             ;; If it worked, check that the variables match
             (if (= b2 fail)
               (segment-match pattern input bindings (+ pos 1) variable?)
               b2))))))))

(defn segment-match+
  "Match one or more elements of input."
  [pattern input bindings variable?]
  (segment-match pattern input bindings 1 variable?))

(defn segment-match?
  "Match zero or one element of input."
  [pattern input bindings variable?]
  (let [var (second (first pattern))
        pat (rest pattern)]
    (or (pat-match (cons var pat) input bindings variable?)
        (pat-match pat input bindings variable?))))

(defn match-if
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  [pattern input bindings variable?]
  (let [result (eval
                 (clojure.walk/postwalk-replace
                   bindings
                   (second (first pattern))))]
    (if (= result false)
      nil
      (pat-match (rest pattern) input bindings variable?))))

(def pat-match-abbrev-map
  "Map of pattern matching abbreviations to their expansions."
  {'?x* '(?* ?x),
   '?y* '(?* ?y)})

(defn pat-match-abbrev
  "Lookup symbol's abbreviation in a map."
  [sym map]
  (let [expansion (sym map)]
    (if (nil? expansion)
      sym
      expansion)))

(defn expand-pat-match-abbrev
  "Expand out all pattern matching abbreviations in pat."
  [map]
  (fn
    [pat]
    (cond (symbol? pat) (pat-match-abbrev pat map)
          (atom? pat) pat
          (empty? pat) '()
          :else (cons
                  ((expand-pat-match-abbrev map) (first pat))
                  ((expand-pat-match-abbrev map) (rest pat))))))

(defn rule-pattern
  [rule]
  (get rule 0))

(defn rule-responses
  [rule]
  (get rule 1))

(defn rule-based-translator
  ([input rules action variable?]
   (rule-based-translator input rules pat-match rule-pattern rule-responses action variable?))
  ([input rules matcher rule-if rule-then action variable?]
   (some
     (fn
       [rule]
       (let [result (funcall
                      matcher
                      (funcall rule-if rule)
                      input
                      variable?)]
         (if (not= result fail)
           (funcall
             action
             result
             (funcall rule-then rule)))))
     rules)))

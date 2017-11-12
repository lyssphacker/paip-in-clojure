(ns ^{:doc "Pattern matcher from section 6.2"}
paip.patmatch
  (:require [paip.auxfns :refer (variable? fail no-bindings
                                           match-variable cons?)]))
(declare segment-match)
(defn segment-match
  [pattern input bindings]
  input)

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
    (resolve (x pattern-fn-map))))

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
  [pattern input bindings]
  ((match-fn (first (first pattern)))
    pattern input bindings))

(declare single-matcher)

(defn pat-match
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

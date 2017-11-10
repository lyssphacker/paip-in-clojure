(ns ^{:doc "Basic version of the Eliza program"}
paip.eliza1
  (:require [clojure.inspector :refer (atom?)]
            [paip.auxfns :refer (match-variable no-bindings fail variable?
                                                cons? position starts-with)]))

(defn segment-pattern?
  "Is this a segment matching pattern: ((?* var) . pat)"
  [pattern]
  (and (cons? pattern)
       (starts-with (first pattern) '?*)))

(declare pat-match)

(defn segment-match
  ([pattern input bindings]
   (segment-match pattern input bindings 0))
  ([pattern input bindings start]
   (let [var (second (first pattern))
         pat (rest pattern)]
     (if (empty? pat)
       (match-variable var input bindings)
       ;; We assume that pat starts with a constant
       ;; In other words, a pattern can't have 2 consecutive vars
       (let [pos (position input (first pat) start)]
         (if (= -1 pos)
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

(defn pat-match
  ([pattern input]
   (pat-match pattern input no-bindings))
  ([pattern input bindings]
   (cond (= bindings fail) fail
         (variable? pattern) (match-variable pattern input bindings)
         (= pattern input) bindings
         (segment-pattern? pattern) (segment-match pattern input bindings)
         (and
           (cons? pattern)
           (cons? input)) (pat-match (rest pattern)
                                     (rest input)
                                     (pat-match (first pattern)
                                                (first input)
                                                bindings))
         :else fail)))

;;; ==============================

(def eliza-rules
  {'((?* ?x) hello (?* ?y))
   ['(How do you do. Please state your problem.)],
   '((?* ?x) I want (?* ?y))
   ['(What would it mean if you got ?y),
    '(Why do you want ?y), '(Suppose you got ?y soon)],
   '((?* ?x) if (?* ?y))
   ['(Do you really think its likely that ?y),
    '(Do you wish that ?y), '(What do you think about ?y),
    '(Really-- if ?y)],
   '((?* ?x) no (?* ?y))
   ['(Why not?), '(You are being a bit negative),
    '(Are you saying "NO" just to be negative?)],
   '((?* ?x) I was (?* ?y)),
   ['(Were you really?), '(Perhaps I already knew you were ?y),
    '(Why do you tell me you were ?y now?)],
   '((?* ?x) I feel (?* ?y)),
   ['(Do you often feel ?y ?)],
   '((?* ?x) I felt (?* ?y)),
   ['(What other feelings do you have?)]})
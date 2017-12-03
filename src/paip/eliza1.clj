(ns ^{:doc "Basic version of the Eliza program"}
paip.eliza1
  (:require [clojure.inspector :refer (atom?)]
            [paip.auxfns :refer (match-variable no-bindings fail variable?
                                                cons? position starts-with
                                                random-elt)]
            [clojure.walk :refer (postwalk-replace)]
            [clojure.pprint :refer (pprint)]
            [paip.patmatch :refer (rule-based-translator rule-pattern rule-responses)]))

(defn segment-pattern?
  "Is this a segment matching pattern: ((?* var) . pat)"
  [pattern]
  (and (cons? pattern)
       (starts-with (first pattern) '?*)))

(declare pat-match)

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
  "Match pattern against input in the context of the bindings"
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
   '((?* ?x) I felt (?* ?y))
   ['(What other feelings do you have?)]})

(defn switch-viewpoint
  "Change I to you and vice versa, and so on."
  [words]
  (postwalk-replace
    {'I 'you, 'you 'I, 'me 'you, 'am 'are}
    words))

(defn use-eliza-rules
  "Find some rule with which to transform the input."
  ([input]
   (use-eliza-rules input eliza-rules))
  ([input rules]
   (some
     (fn
       [rule]
       (let [result
             (pat-match
               (rule-pattern rule)
               input)]
         (if
           (not= result fail)
           (postwalk-replace
             (switch-viewpoint result)
             (random-elt (rule-responses rule))))))
     rules)))

(defn use-eliza-rules
  "Find some rule with which to transform the input."
  ([input]
   (use-eliza-rules input eliza-rules))
  ([input rules]
   (rule-based-translator
     input
     eliza-rules
     (fn
       [bindings responses]
       (postwalk-replace
         (switch-viewpoint bindings)
         (random-elt responses)))
     variable?)))

(defn eliza
  "Respond to user input using pattern matching rules."
  []
  (loop []
    (println 'eliza>)
    (let [input (read)]
      (if (= input 'quit)
        'done
        (do
          (pprint
            (flatten
              (use-eliza-rules input)))
          (recur))))))

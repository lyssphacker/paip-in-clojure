(ns ^{:doc "The implementation of MACSYMA"}
paip.macsyma
  (:require [better-cond.core :as b]
            [paip.patmatch :refer :all]
            [paip.macsymar :refer :all]
            [clojure.math.numeric-tower :as math]
            [clojure.inspector :refer (atom?)]
            [clojure.walk :refer (postwalk-replace)]
            [paip.auxfns :refer (fmap-values member mappend)]
            [paip.student :refer (prefix->infix
                                   binary-exp?
                                   rule-pat rule-res
                                   exp-args exp? mkexp
                                   exp-lhs exp-rhs exp-op)]))

(defn not-number?
  [x]
  (not (number? x)))

(def macsyma-pat-match-abbrev-map
  "Map of pattern matching abbreviations to their expansions."
  {'x+ '(?+ x)
   'y+ '(?+ y)
   'n  '(?is n number?)
   'm  '(?is m number?)
   's  '(?is s not-number?)})

(def infix->prefix-rules
  (map (expand-pat-match-abbrev macsyma-pat-match-abbrev-map)
       '(((x+ = y+) (= x y))
          ((- x+) (- x))
          ((+ x+) (+ x))
          ((x+ + y+) (+ x y))
          ((x+ - y+) (- x y))
          ((x+ * y+) (* x y))
          ((x+ / y+) (/ x y))
          ((x+ expt y+) (expt x y)))))

(defn variable?
  "Variables are the symbols M through Z."
  ;; put x,y,z first to find them a little faster
  [exp]
  (member
    '(x y z m n o p q r s t u v w)
    exp))

(defn infix->prefix
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  [exp]
  (b/cond
    (atom? exp) exp
    (= (count exp) 1) (infix->prefix (first exp))
    :let [res
          (rule-based-translator
            exp
            infix->prefix-rules
            pat-match
            rule-pat
            rule-res
            (fn
              [bindings response]
              (postwalk-replace
                (fmap-values bindings infix->prefix)
                response))
            variable?)]
    (not (nil? res)) res
    (symbol? (first exp)) (list (first exp) (infix->prefix (rest exp)))
    :else (throw (Exception. "Illegal exp"))))

(defn expt
  "Exponentiation"
  [x y]
  (math/expt x y))

(defn simp-rule
  "Transform a rule into proper format."
  [rule]
  (let [exp (infix->prefix rule)]
    (mkexp
      ((expand-pat-match-abbrev macsyma-pat-match-abbrev-map) (exp-lhs exp))
      (exp-op exp)
      (exp-rhs exp))))

(defn simplification-rules
  ([]
   (simplification-rules basic-rules))
  ([& rules]
    (mappend
      #(map simp-rule %)
      rules)))

(declare simplify-exp)

(defn simplify
  "Simplify an expression by first simplifying its components."
  [exp]
  (if
    (atom? exp)
    exp
    (simplify-exp
      (map simplify exp))))

(defn evaluable
  "Is this an arithmetic expression that can be evaluated?"
  [exp]
  (and
    (every? number? (exp-args exp))
    (or (member '(+ - * /) (exp-op exp))
        (and (= (exp-op exp) 'expt)
             (integer? (second (exp-args exp)))))))

(defn simplify-exp
  "Simplify using a rule, or by doing arithmetic."
  [exp]
  (b/cond
    (atom? exp) exp
    (= (count exp) 1) (infix->prefix (first exp))
    :let [res
          (rule-based-translator
            exp
            (simplification-rules)
            pat-match
            exp-lhs
            exp-rhs
            (fn
              [bindings response]
              (simplify
                (postwalk-replace bindings response)))
            variable?)]
    (not (nil? res)) res
    (evaluable exp) (eval exp)
    :else exp))

(defn simp
  [inf]
  (prefix->infix
    (simplify
      (infix->prefix inf))))

(defn simplifier
  "Read a mathematical expression, simplify it, and print the result."
  []
  (loop []
    (print 'simplifier>)
    (let [input (read-string (read-line))]
      (if (= input 'quit)
        'done
        (do
          (println (simp input))
          (recur))))))


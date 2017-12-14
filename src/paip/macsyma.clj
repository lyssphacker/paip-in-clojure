(ns ^{:doc "The implementation of MACSYMA"}
paip.macsyma
  (:require [better-cond.core :as b]
            [paip.patmatch :refer :all]
            [paip.macsymar :refer :all]
            [clojure.math.numeric-tower :as math]
            [clojure.inspector :refer (atom?)]
            [clojure.walk :refer (postwalk-replace)]
            [paip.auxfns :refer (fmap-values member mappend
                                             funcall starts-with
                                             length=1 find-first)]
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
          ((d y+ / d x) (d y x))                            ;*** New rule
          ((Int y+ d x) (int y x))                          ;*** New rule
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

(defn get-simplification-rules
  ([]
   (get-simplification-rules basic-rules associativity-commutativity-rules
                             logs-trigs-rules differentiation-rules))
  ([& rules]
   (mappend
     #(map simp-rule %)
     rules)))

(def simplification-rules (get-simplification-rules))

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

(declare simplify-by-fn)

(defn simplify-exp
  "Simplify using a rule, or by doing arithmetic."
  [exp]
  (b/cond
    :let [res (simplify-by-fn exp)]
    (not (nil? res)) res
    (atom? exp) exp
    (= (count exp) 1) (infix->prefix (first exp))
    :let [res
          (rule-based-translator
            exp
            simplification-rules
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

(declare unfactorize)
(defn factorize
  [exp]
  (with-local-vars [factors nil
        constant 1]
    (letfn [(fac [x n]
              (cond
                (number? x) (var-set constant
                                     (* @constant (expt x n)))
                (starts-with x '*) (do
                                     (fac (exp-lhs x) n)
                                     (fac (exp-rhs x) n))
                (starts-with x '/) (do
                                     (fac (exp-lhs x) n)
                                     (fac (exp-rhs x) (- n)))
                (and (starts-with x '-)
                     (length=1 (exp-args x)))
                (var-set constant (- @constant))
                (fac (exp-lhs x) n)
                (and (starts-with x 'expt)
                     (number? (exp-rhs x))) (fac (exp-lhs x)
                                                 (* n (exp-rhs x)))
                :else (let [factor (find-first #(= x (exp-lhs %)) factors)]
                        (if factor
                          ()))))])))
(declare integrate)

(def simp-fn-map
  {'Int
   (fn [exp]
     (unfactorize
       (factorize
         (integrate
           (exp-lhs exp)
           (exp-rhs exp)))))})

(defn simplify-by-fn
  [exp]
  (let [fn ((exp-op exp) simp-fn-map)
        result (if fn (funcall fn exp))]
    (if (nil? result)
      nil
      (simplify result))))
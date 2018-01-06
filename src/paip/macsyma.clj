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
                                             length=1 find-first-index
                                             eql? subst)]
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

(defn unfactorize
  "Convert a list of factors back into prefix form."
  [factors]
  (cond
    (empty? factors) 1
    (length=1 factors) (first factors)
    :else `(* ~(first factors) ~(unfactorize (rest factors)))))

(defn factorize
  "Return a list of the factors of exp^n,
  where each factor is of the form (^ y n)."
  [exp]
  (with-local-vars [factors []
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
                (do
                  (var-set constant (- @constant))
                  (fac (exp-lhs x) n))
                (and (starts-with x 'expt)
                     (number? (exp-rhs x))) (fac (exp-lhs x)
                                                 (* n (exp-rhs x)))
                :else (let [index (find-first-index #(= x (exp-lhs %)) factors)]
                        (if (not= index -1)
                          (var-set factors
                                   (update-in @factors
                                              (vec index)
                                              #(mkexp (exp-lhs %)
                                                  (exp-op %)
                                                  (+ (exp-rhs %) n))))
                          (var-set factors
                                   (cons `(expt ~x ~n)
                                         @factors))))))]
      (fac exp 1)
      (case constant
        0 '((expt 0 1))
        1 factors
        `((expt ~constant 1) ~@factors)))))
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

(defn divide-factors
  "Divide a list of factors by another, producing a third."
  [numer denom]
  (with-local-vars [result numer]
    (doseq [d denom]
      (let [index (find-first-index
                     #(= (exp-lhs d) (exp-lhs %))
                     result)]
        (if (not= index -1)
          (var-set
            result
            (update-in
              @result
              (vec index)
              #(mkexp (exp-lhs %)
                      (exp-op %)
                      (- (exp-rhs %) (exp-rhs d)))))
          (var-set
            result
            (cons `(expt ~(exp-lhs d) ~(- (exp-rhs d)))
                  @result)))))
    (filter #(= (exp-rhs %) 0) @result)))

(defn find-anywhere
  "Does item occur anywhere in tree?  If so, return it."
  [item tree]
  (b/cond
    (eql? item tree) tree
    (or (and (coll? tree) (empty? tree))
        (atom? tree)) nil
    :let [res (find-anywhere item (first tree))]
    (not (nil? res)) res
    :else (find-anywhere item (rest tree))))

(defn free-of
  "True if expression has no occurrence of var."
  [exp var]
  (not (find-anywhere var exp)))

(defn partition-if
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  [pred list]
  (with-local-vars [yes-list '()
                    no-list '()]
    (doseq [item list]
      (if (funcall pred item)
        (var-set yes-list (cons item @yes-list))
        (var-set no-list (cons item @no-list))))
    [(reverse @yes-list) (reverse @no-list)]))

(defn deriv
  [y x]
  (simplify `(d ~y ~x)))

(defn create-integration-table
  [rules]
  (with-local-vars [table {}]
    (doseq [i-rule rules]
      ;; changed infix->prefix to simp-rule - norvig Jun 11 1996
      (let [rule (simp-rule i-rule)]
        (var-set table
                 (assoc @table
                   (exp-op (exp-lhs (exp-lhs rule)))
                   rule))))
    table))

(def integration-table
  (create-integration-table integration-table-rules))

(defn in-integral-table?
  [exp]
  (and (exp? exp) ((exp-op exp) integration-table)))

(defn integrate-from-table
  [op arg]
  (let [rule (op integration-table)]
    (subst arg
           (exp-lhs (exp-lhs (exp-lhs rule)))
           (exp-rhs rule))))

(defn deriv-divides
  [factor factors x]
  (assert (starts-with factor 'expt))
  (let [u (exp-lhs factor)
        n (exp-rhs factor)
        k (divide-factors
            factors (factorize `(* ~factor ~(deriv u x))))]
    (cond
      (free-of k x)
      ;; Int k*u^n*du/dx dx = k*Int u^n du
      ;;                    = k*u^(n+1)/(n+1) for n/=-1
      ;;                    = k*log(u) for n=-1
      (if (= n 1)
        `(* ~(unfactorize k) (log ~u))
        `(/ (* ~(unfactorize k) (^ ~u ~(+ n 1)))
            ~(+ n 1)))
      (and (= n 1) (in-integral-table? u))
      ;; Int y'*f(y) dx = Int f(y) dy
      (let [k2 (divide-factors
                 factors
                 (factorize `(* ~u ~(deriv (exp-lhs u) x))))]
        (if (free-of k2 x)
          `(* ~(integrate-from-table (exp-op u) (exp-lhs u))
              ~(unfactorize k2)))))))

(defn integrate
  [exp x]
  (b/cond
    ;; First try some trivial cases
    (free-of exp x) `(* ~exp x) ; Int c dx = c*x
    (starts-with exp '+) `(+ ~(integrate (exp-lhs exp) x) ; Int f + g  =
                             ~(integrate (exp-rhs exp) x)) ; Int f + Int g
    (starts-with exp '-)
    (case (count (exp-args exp))
      1 (integrate (exp-lhs exp) x) ; Int - f = - Int f
      2 `(- ~(integrate (exp-lhs exp) x) ; Int f - g  =
            ~(integrate (exp-rhs exp) x))) ; Int f - Int g
    ;; Now move the constant factors to the left of the integral
    :else (let [[const-factors x-factors]
              (partition-if
                (fn [factor]
                  (free-of factor x))
                (factorize exp))]
              (identity ;simplify
                `(* ~(unfactorize const-factors)
                    ;; And try to integrate:
                    ~(cond
                       (empty? x-factors) x
                       :let [res (some
                                   (fn [factor]
                                     (deriv-divides factor x-factors x))
                                   x-factors)]
                       (not (nil? res)) res
                       ;; <other methods here>
                       :else `(int? ~(unfactorize x-factors) ~x)))))))
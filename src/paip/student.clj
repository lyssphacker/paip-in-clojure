(ns ^{:doc "Chapter 7's STUDENT program to solve algebra word problems."}
paip.student
  (:require [paip.patmatch :refer :all]
            [paip.auxfns :refer (fmap remove-if fmap-values in?
                                      subst cons?)]
            [clojure.walk :refer (postwalk-replace)]
            [clojure.inspector :refer (atom?)]
            [clojure.pprint :refer (cl-format)]))


(def student-rules-abbrev
  {'(?x* .)                          '?x,
   '(?x* . ?y*)                      '(?x ?y),
   '(if ?x* \, then ?y*)             '(?x ?y),
   '(if ?x* then ?y*)                '(?x ?y),
   '(if ?x* \, ?y*)                  '(?x ?y),
   '(?x* \, and ?y*)                 '(?x ?y),
   '(find ?x* and ?y*)               '((= to-find-1 ?x) (= to-find-2 ?y)),
   '(find ?x*)                       '(= to-find ?x),
   '(?x* equals ?y*)                 '(= ?x ?y),
   '(?x* same as ?y*)                '(= ?x ?y),
   '(?x* = ?y*)                      '(= ?x ?y),
   '(?x* is equal to ?y*)            '(= ?x ?y),
   '(?x* is ?y*)                     '(= ?x ?y),
   '(?x* - ?y*)                      '(- ?x ?y),
   '(?x* minus ?y*)                  '(- ?x ?y),
   '(difference between ?x* and ?y*) '(- ?y ?x),
   '(difference ?x* and ?y*)         '(- ?y ?x),
   '(?x* + ?y*)                      '(+ ?x ?y),
   '(?x* plus ?y*)                   '(+ ?x ?y),
   '(sum ?x* and ?y*)                '(+ ?x ?y),
   '(product ?x* and ?y*)            '(* ?x ?y),
   '(?x* * ?y*)                      '(* ?x ?y),
   '(?x* times ?y*)                  '(* ?x ?y),
   '(?x* / ?y*)                      '(/ ?x ?y),
   '(?x* per ?y*)                    '(/ ?x ?y),
   '(?x* divided by ?y*)             '(/ ?x ?y),
   '(half ?x*)                       '(/ ?x 2),
   '(one half ?x*)                   '(/ ?x 2),
   '(twice ?x*)                      '(* 2 ?x),
   '(square ?x*)                     '(* ?x ?x),
   '(?x* % less than ?y*)            '(* ?y (/ (- 100 ?x) 100)),
   '(?x* % more than ?y*)            '(* ?y (/ (+ 100 ?x) 100)),
   '(?x* % ?y*)                      '(* (/ ?x 100) ?y)})

(defn exp-args
  [exp]
  (rest exp))

(defn exp?
  [x]
  (cons? x))

(defn mkexp
  [lhs op rhs]
  (list lhs op rhs))

(defn exp-lhs [exp] (nth exp 0))
(defn exp-op [exp] (nth exp 1))
(defn exp-rhs [exp] (nth exp 2))

(def student-rules
  (fmap
    student-rules-abbrev
    expand-pat-match-abbrev))

(defn rule-pat [rule] (first rule))
(defn rule-res [rule] (second rule))

(declare translate-to-expression)

(defn translate-pair
  "Translate the value part of the pair into an equation or expression."
  [bindings]
  (fmap-values bindings translate-to-expression))

(defn make-variable
  "Create a variable name based on the given list of words"
  [words]
  (first words))

(defn translate-to-expression
  "Translate an English phrase into an equation or expression."
  [words]
  (or
    (rule-based-translator
      words
      student-rules
      pat-match
      rule-pat
      rule-res
      (fn
        [bindings response]
        (postwalk-replace
          (translate-pair bindings)
          response)))
    (make-variable words)))

(defn noise-words?
  "Is this a low-content word which can be safely ignored?"
  [word]
  (in? word '(a an the this number of $)))

(defn create-list-of-equations
  "Separate out equations embedded in nested parens."
  [exp]
  (cond (empty? exp) '()
        (atom? (first exp)) (list exp)
        :else (concat
                (create-list-of-equations (first exp))
                (create-list-of-equations (rest exp)))))

(declare binary-exp?)

(defn prefix->infix
  [exp]
  (if (atom? exp)
    exp
    (map
      prefix->infix
      (if (binary-exp? exp)
        (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
        exp))))

(defn print-equations
  [header equations]
  (cl-format
    true
    "~%~a~{~%  ~{ ~a~}~}~%"
    header
    (map prefix->infix equations)))

(defn solve-equations
  [equations]
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" (solve equations '())))

(defn solve-arithmetic
  [equation]
  (mkexp
    (exp-lhs equation)
    '=
    (eval (exp-rhs equation))))

(defn unknown? [exp] (symbol? exp))

(defn in-exp?
  "True if x appears anywhere in exp"
  [x exp]
  (or
    (= x exp)
    (and
      (map? exp)
      (or
        (in-exp? x (exp-lhs exp))
        (in-exp? x (exp-rhs exp))))))

(defn no-unknown?
  "Returns true if there are no unknowns in exp."
  [exp]
  (cond (unknown? exp) false
        (atom? exp) true
        (no-unknown? (exp-lhs exp)) (no-unknown? (exp-rhs exp))
        :else false))

(defn one-unknown
  "Returns the single unknown in exp, if there is exactly one."
  [exp]
  (cond (unknown? exp) exp
        (atom? exp) nil
        (no-unknown? (exp-lhs exp)) (one-unknown (exp-rhs exp))
        (no-unknown? (exp-rhs exp)) (one-unknown (exp-lhs exp))
        :else nil))

(declare isolate)

(defn solve
  "Solve a system of equations by constraint propagation."
  ;; Try to solve for one equation, and substitute its value into
  ;; the others. If that doesn't work, return what is known.
  [equations known]
  (or
    (some
      (fn [equation]
        (let [x (one-unknown equation)]
          (when x
            (let [answer (solve-arithmetic
                           (isolate equation x))]
              (solve
                (subst
                  (exp-rhs answer)
                  (exp-lhs answer)
                  (remove
                    #(= equation %1)
                    equations))
                (cons answer known))))))
      equations)
    known))

(defn binary-exp?
  [x]
  (and
    (exp? x)
    (= (count (exp-args x)) 2)))

(defn student
  "Solve certain Algebra Word Problems."
  [words]
  (solve-equations
    (create-list-of-equations
      (translate-to-expression
        (remove-if
          noise-words?
          words)))))


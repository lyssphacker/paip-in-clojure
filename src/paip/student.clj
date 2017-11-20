(ns ^{:doc "Chapter 7's STUDENT program to solve algebra word problems."}
paip.student
  (:require [paip.patmatch :refer :all]
            [paip.auxfns :refer (fmap remove-if fmap-values in?
                                      subst)]
            [clojure.walk :refer (postwalk-replace)]
            [clojure.inspector :refer (atom?)]))


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

(declare solve)
(declare print-equations)

(defn mkexp
  [lhs op rhs]
  {:lhs lhs :op op :rhs rhs})

(defn solve-equations
  [equations]
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" (solve equations '())))

(defn solve-arithmetic
  [equation]
  (mkexp
    (:lhs equation)
    '=
    (eval (:rhs equation))))

(defn unknown? [exp] (symbol? exp))

(defn in-exp?
  "True if x appears anywhere in exp"
  [x exp]
  (or
    (= x exp)
    (and
      (map? exp)
      (or
        (in-exp? x (:lhs exp))
        (in-exp? x (:rhs exp))))))

(defn no-unknown?
  "Returns true if there are no unknowns in exp."
  [exp]
  (cond (unknown? exp) false
        (atom? exp) true
        (no-unknown? (:lhs exp)) (no-unknown? (:rhs exp))
        :else false))

(defn one-unknown
  "Returns the single unknown in exp, if there is exactly one."
  [exp]
  (cond (unknown? exp) exp
        (atom? exp) nil
        (no-unknown? (:lhs exp)) (one-unknown (:rhs exp))
        (no-unknown? (:rhs exp)) (one-unknown (:lhs exp))
        :else nil))

(declare isolate)

(defn solve
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
                  (:rhs answer)
                  (:lhs answer)
                  (remove
                    #(= equation %1)
                    equations))
                (cons answer known))))))
      equations)
    known))

(defn student
  "Solve certain Algebra Word Problems."
  [words]
  (solve-equations
    (create-list-of-equations
      (translate-to-expression
        (remove-if
          noise-words?
          words)))))


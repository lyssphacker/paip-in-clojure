(ns ^{:doc "Chapter 7's STUDENT program to solve algebra word problems."}
paip.student
  (:require [paip.patmatch :refer (pat-match expand-pat-match-abbrev
                                             rule-based-translator)]
            [paip.auxfns :refer (fmap remove-if fmap-values in?)]
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

(declare solve-equations)

(defn rule-pattern [rule] (get 0 rule))
(defn rule-response [rule] (get 1 rule))

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
      rule-pattern
      rule-response
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

(defn student
  "Solve certain Algebra Word Problems."
  [words]
  (create-list-of-equations
    (translate-to-expression
      (remove-if
        noise-words?
        words))))


(ns ^{:doc "Chapter 7's STUDENT program to solve algebra word problems."}
paip.student
  (:require [paip.patmatch :refer :all]
            [paip.auxfns :refer (fmap remove-if fmap-values in?
                                      subst cons? mapcar)]
            [clojure.walk :refer (postwalk-replace)]
            [clojure.inspector :refer (atom?)]
            [clojure.pprint :refer (cl-format)]))


(def student-rules-abbrev
  '(((?x* .) ?x)
     ((?x* . ?y*) (?x ?y))
     ((if ?x* \, then ?y*) (?x ?y))
     ((if ?x* then ?y*) (?x ?y))
     ((if ?x* \, ?y*) (?x ?y))
     ((?x* \, and ?y*) (?x ?y))
     ((find ?x* and ?y*) ((= to-find-1 ?x) (= to-find-2 ?y)))
     ((find ?x*) (= to-find ?x))
     ((?x* equals ?y*) (= ?x ?y))
     ((?x* same as ?y*) (= ?x ?y))
     ((?x* = ?y*) (= ?x ?y))
     ((?x* is equal to ?y*) (= ?x ?y))
     ((?x* is ?y*) (= ?x ?y))
     ((?x* - ?y*) (- ?x ?y))
     ((?x* minus ?y*) (- ?x ?y))
     ((difference between ?x* and ?y*) (- ?y ?x))
     ((difference ?x* and ?y*) (- ?y ?x))
     ((?x* + ?y*) (+ ?x ?y))
     ((?x* plus ?y*) (+ ?x ?y))
     ((sum ?x* and ?y*) (+ ?x ?y))
     ((product ?x* and ?y*) (* ?x ?y))
     ((?x* * ?y*) (* ?x ?y))
     ((?x* times ?y*) (* ?x ?y))
     ((?x* / ?y*) (/ ?x ?y))
     ((?x* per ?y*) (/ ?x ?y))
     ((?x* divided by ?y*) (/ ?x ?y))
     ((half ?x*) (/ ?x 2))
     ((one half ?x*) (/ ?x 2))
     ((twice ?x*) (* 2 ?x))
     ((square ?x*) (* ?x ?x))
     ((?x* % less than ?y*) (* ?y (/ (- 100 ?x) 100)))
     ((?x* % more than ?y*) (* ?y (/ (+ 100 ?x) 100)))
     ((?x* % ?y*) (* (/ ?x 100) ?y))))

(defn exp-args
  [exp]
  (rest exp))

(defn exp?
  [x]
  (cons? x))

(defn mkexp
  [lhs op rhs]
  (list op lhs rhs))

(defn exp-lhs [exp] (nth exp 1))
(defn exp-op [exp] (nth exp 0))
(defn exp-rhs [exp] (nth exp 2))

(def student-rules
  (map
    expand-pat-match-abbrev
    student-rules-abbrev))

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

(defn binary-exp?
  [x]
  (and
    (exp? x)
    (= (count (exp-args x)) 2)))

(defn prefix->infix
  "Translate prefix to infix expressions."
  [exp]
  (if (atom? exp)
    exp
    (map
      prefix->infix
      (if (binary-exp? exp)
        (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
        exp))))

(defn print-equations
  "Print a list of equations."
  [header equations]
  (cl-format
    true
    "~%~a~{~%  ~{ ~a~}~}~%"
    header
    (map prefix->infix equations)))

(defn solve-arithmetic
  "Do the arithmetic for the right hand side."
  ;; This assumes that the right hand side is in the right form.
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
      (cons? exp)
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

(def operators-and-inverses
  {'+ '-
   '- '+
   '* '/
   '/ '*
   '= '=})

(defn inverse-op [op] (op operators-and-inverses))

(defn commutative?
  "Is operator commutative?"
  [op]
  (in? op '(+ * =)))

(defn isolate
  [e x]
  "Isolate the lone x in e on the left hand side of e."
  ;; This assumes there is exactly one x in e,
  ;; and that e is an equation.
  (cond (= (exp-lhs e) x)
        ;; Case I: X = A -> X = n
        e
        (in-exp? x (exp-rhs e))
        ;; Case II: A = f(X) -> f(X) = A
        (isolate (mkexp (exp-rhs e) '= (exp-lhs e)) x)
        (in-exp? x (exp-lhs (exp-lhs e)))
        ;; Case III: f(X)*A = B -> f(X) = B/A
        (isolate (mkexp (exp-lhs (exp-lhs e)) '=
                        (mkexp (exp-rhs e)
                               (inverse-op (exp-op (exp-lhs e)))
                               (exp-rhs (exp-lhs e)))) x)
        (commutative? (exp-op (exp-lhs e)))
        ;; Case IV: A*f(X) = B -> f(X) = B/A
        (isolate (mkexp (exp-rhs (exp-lhs e)) '=
                        (mkexp (exp-rhs e)
                               (inverse-op (exp-op (exp-lhs e)))
                               (exp-lhs (exp-lhs e)))) x)
        :else
        ;; Case V: A/f(X) = B -> f(X) = A/B
        (isolate (mkexp (exp-rhs (exp-lhs e)) '=
                        (mkexp (exp-lhs (exp-lhs e))
                               (exp-op (exp-lhs e))
                               (exp-rhs e))) x)))

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

(defn solve-equations
  "Print the equations and their solution"
  [equations]
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" (solve equations '())))

(defn student
  "Solve certain Algebra Word Problems."
  [words]
  (solve-equations
    (create-list-of-equations
      (translate-to-expression
        (remove-if
          noise-words?
          words)))))
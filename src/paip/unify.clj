(ns ^{:doc "Unification functions"}
  paip.unify
  (:require [paip.patmatch :refer :all]
            [paip.auxfns :refer (no-bindings variable? eql?
                                             cons? match-variable
                                             fail extend-bindings)]
            [clojure.inspector :refer (atom?)]))

(def occurs-check  "Should we do the occurs check?" true)

(defn check-occurs
  "Does var occur anywhere inside x?"
  [variable x bindings]
  (cond (= variable x)
        true
        (and (variable? x) (contains? bindings x))
        (check-occurs variable (bindings x) bindings)
        (cons? x)
        (or (check-occurs variable (first x) bindings)
            (check-occurs variable (rest x) bindings))
        :else false))

(declare unify)

(defn unify-variable
  "Unify var with x, using (and maybe extending) bindings."
  [variable x bindings]
  (cond (contains? bindings variable)
        (unify (variable bindings) x bindings)
        (and (variable? x) (contains? bindings x))
        (unify variable (x bindings) bindings)
        (and occurs-check (check-occurs variable x bindings))
        fail
        :else (extend-bindings variable x bindings)))

(defn unify
  "See if x and y match with given bindings."
  ([x y]
   (unify x y no-bindings))
  ([x y bindings]
   (cond (= bindings fail) fail
         (eql? x y) bindings
         (variable? x) (unify-variable x y bindings)
         (variable? y) (unify-variable y x bindings)
         (and
           (cons? x)
           (cons? y)) (unify (rest x)
                             (rest y)
                             (unify (first x)
                                    (first y)
                                    bindings))
         :else fail)))

(defn subst-bindings
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  [bindings x]
  (cond (= bindings fail)
        fail
        (= bindings no-bindings)
        x
        (and (variable? x) (bindings x))
        (subst-bindings bindings (bindings x))
        (atom? x) x
        :else (cons
                (subst-bindings bindings (first x))
                (subst-bindings bindings (rest x)))))
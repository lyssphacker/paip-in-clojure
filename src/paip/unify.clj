(ns ^{:doc "Unification functions"}
  paip.unify
  (:require [paip.auxfns :refer (no-bindings variable? eql?
                                             cons? match-variable
                                             fail extend-bindings
                                             starts-with)]
            [clojure.inspector :refer (atom?)]
            [clojure.string :refer (starts-with?)]))

(def occurs-check  "Should we do the occurs check?" true)

(defn check-occurs
  "Does var occur anywhere inside x?"
  [variable x bindings]
  (cond (= variable x)
        true
        (and (variable? x) (contains? bindings x))
        (check-occurs variable (bindings x) bindings)
        (and (cons? x) (not (empty? x)))
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

(defn cdr-variable?
  [x]
  (and (seq? x)
       (seq? (first x))
       (symbol? (first (first x)))
       (= (name (first (first x))) "?*")))

(defn unify
  "See if x and y match with given bindings."
  ([x y]
   (unify x y no-bindings))
  ([x y bindings]
   (cond (= bindings fail) fail
         (eql? x y) bindings
         (variable? x) (unify-variable x y bindings)
         (variable? y) (unify-variable y x bindings)
         (cdr-variable? x) (unify-variable (second (first x)) y bindings)
         (cdr-variable? y) (unify-variable (second (first y)) x bindings)
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
        (or (atom? x) (empty? x))
        x
        :else (cons
                (subst-bindings bindings (first x))
                (subst-bindings bindings (rest x)))))

(defn unifier
  "Return something that unifies with both x and y (or fail)."
  [x y]
  (subst-bindings (unify x y) x))
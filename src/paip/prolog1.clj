(ns ^{:doc "First version of the prolog interpreter (11.2)."}
  paip.prolog1
  (:require [paip.unify :refer :all]
            [paip.auxfns :refer (variable? prepend)]))

(defn clause-head [clause] (first clause))
(defn clause-body [clause] (rest clause))

(def clauses {})

(def db-predicates '())

(defn get-clauses [pred] (clauses pred))
(defn predicate [relation] (first relation))

(defn add-clause
  [clause]
  (let [pred (predicate (clause-head clause))]
    (assert (and (symbol? pred) (not (variable? pred))))
    (alter-var-root
      #'db-predicates
          (fn [p]
            (prepend pred p)))
    (alter-var-root
      #'clauses
      (fn [m]
        (assoc m pred (concat (pred m) (list clause)))))
    pred))

(defmacro <- [& clause]
  `(add-clause '~clause))

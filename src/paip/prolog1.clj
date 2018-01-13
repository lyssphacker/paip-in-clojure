(ns ^{:doc "First version of the prolog interpreter (11.2)."}
  paip.prolog1
  (:require [paip.unify :refer :all]
            [paip.auxfns :refer (variable? prepend
                                           funcall adjoin)]
            [clojure.inspector :refer (atom?)]))

(defn clause-head [clause] (first clause))
(defn clause-body [clause] (rest clause))

(def clauses {})

(def db-predicates '())

(defn get-clauses [pred] (clauses pred))
(defn predicate [relation] (first relation))

(defn add-clause
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  [clause]
  (let [pred (predicate (clause-head clause))]
    (assert (and (symbol? pred) (not (variable? pred))))
    (alter-var-root
      #'db-predicates
          (fn [p]
            (prepend (list pred) p)))
    (alter-var-root
      #'clauses
      (fn [m]
        (assoc m pred (concat (pred m) (list clause)))))
    pred))

(defmacro <- [& clause]
  "Add a clause to the data base."
  `(add-clause '~clause))

(defn clear-predicate
  [predicate]
  (alter-var-root
    #'clauses
    (fn [c]
      (dissoc c predicate))))

(defn clear-db
  "Remove all clauses (for all predicates) from the data base."
  []
  (doseq [predicate db-predicates]
    (clear-predicate predicate)))

(defn unique-find-anywhere-if
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  ([predicate tree]
    (unique-find-anywhere-if predicate tree '()))
  ([predicate tree found-so-far]
    (if (or (atom? tree) (empty? tree))
      (if (funcall predicate tree)
        (adjoin found-so-far tree)
        found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if
          predicate
          (rest tree)
          found-so-far)))))

(defn variables-in
  "Return a list of all the variables in EXP."
  [exp]
  (unique-find-anywhere-if variable? exp))


(ns ^{:doc "First version of the prolog interpreter (11.2)."}
  paip.prolog1
  (:require [paip.unify :refer :all]
            [paip.auxfns :refer (variable? prepend
                                           funcall adjoin
                                           mapcan fail
                                           mapcar)]
            [clojure.inspector :refer (atom?)]
            [clojure.walk :refer (postwalk-replace)]))

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

(defn rename-variables
  "Replace all variables in x with new ones."
  [x]
  (postwalk-replace
    (mapcar
      (fn [var] {var (gensym var)})
      (variables-in x))
    x))

(declare prove-all)

(defn prove
  "Return a list of possible solutions to goal."
  [goal bindings]
  (mapcan
    (fn [clause]
      (let [new-clause (rename-variables clause)]
        (prove-all (clause-body new-clause)
                   (unify goal (clause-head new-clause)))))
    (clauses (predicate goal))))

(defn prove-all
  "Return a list of solutions to the conjunction of goals."
  [goals bindings]
  (cond (= bindings fail) fail
        (empty? goals) (list bindings)
        :else (mapcan
                (fn [goal1-solution]
                  (prove-all
                    (rest goals)
                    goal1-solution))
                (prove (first goals) bindings))))
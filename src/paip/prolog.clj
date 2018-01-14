(ns ^{:doc "Prolog from (11.3), with interactive backtracking."}
  paip.prolog
  (:require [paip.prolog1 :refer (clause-head clause-body
                                             predicate db-predicates
                                             add-clause clear-db
                                             clear-predicate unique-find-anywhere-if
                                             clauses rename-variables)]
            [paip.unify :refer :all]
            [paip.auxfns :refer (funcall fail no-bindings)]
            [clojure.pprint :refer (cl-format)]
            [clojure.walk :refer (postwalk-replace)]))

(declare prove-all)

(defn prove
  [goal bindings other-goals]
  (let [clauses (clauses (predicate goal))]
    (if (seq? clauses)
      (some
        (fn [clause]
          (let [new-clause (rename-variables clause)]
            (prove-all
              (concat (clause-body new-clause) other-goals)
              (unify goal (clause-head new-clause) bindings))))
        clauses)
      (funcall clauses (rest goal) bindings other-goals))))

(defn prove-all
  [goals bindings]
  (cond (= bindings fail) fail
        (empty? goals) bindings
        :else (prove (first goals) bindings (rest goals))))

(defn continue?
  []
  (let [input (read)]
    (cond (= input 't ) true
          (= input 'f) nil
          (= input 'c) (continue?)
          :else (do
                  (cl-format true " Type ; to see more or . to stop")
                  (continue?)))))

(defn show-prolog-vars
  [vars bindings other-goals]
  (if (empty? vars)
    (cl-format true "~&Yes~%")
    (doseq [var vars]
      (cl-format true "~&~a = ~a~%" var
              (postwalk-replace bindings var))))
  (if (continue?)
    fail
    (prove-all other-goals bindings)))

(declare variables-in)

(defn top-level-prove
  [goals]
  (prove-all `(~@goals (show-prolog-vars ~@(variables-in goals)))
             no-bindings)
  (cl-format true "~&No.~%"))


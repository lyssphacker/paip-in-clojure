(ns ^{:doc "Prolog from (11.3), with interactive backtracking."}
  paip.prolog
  (:require [paip.prolog1 :refer (clause-head clause-body
                                             predicate db-predicates
                                             add-clause clear-db
                                             clear-predicate unique-find-anywhere-if)]))

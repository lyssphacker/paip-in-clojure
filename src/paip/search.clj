(ns ^{:doc "Search routines from section 6.4"}
  paip.search
  (:require [paip.auxfns :refer (funcall fail)]))

(defn tree-search
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  [states goal? successors combiner]
  (cond
    (empty? states) fail
    (funcall goal? (first states)) (first states)
    :else (tree-search
            (funcall combiner
                     (funcall successors (first states)
                              (rest states)))
            goal? successors combiner)))


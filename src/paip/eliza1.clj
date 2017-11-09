(ns ^{:doc "Basic version of the Eliza program"}
  paip.eliza1
  (:require [clojure.inspector :refer (atom?)]
            [paip.auxfns :refer (match-variable no-bindings fail variable? cons?)]))

(defn pat-match
  ([pattern input]
   (pat-match pattern input no-bindings))
  ([pattern input bindings]
   (cond (= bindings fail) fail
         (variable? pattern) (match-variable pattern input bindings)
         (= pattern input) bindings
         (and
           (cons? pattern)
           (cons? input)) (pat-match (rest pattern)
                                     (rest input)
                                     (pat-match (first pattern)
                                                (first input)
                                                bindings))
         :else fail)))
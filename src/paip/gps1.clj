(ns ^{:doc "First version of GPS (General Problem Solver)"}
paip.gps1)

(def state
  "The current state: a set of conditions."
  #{})

(def ops
  "A set of available operators."
  #{})

(def op
  "An operation"
  {:action   nil
   :preconds #{}
   :add-set  #{}
   :del-set  #{}})
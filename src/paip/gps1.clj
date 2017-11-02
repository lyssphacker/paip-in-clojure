(ns ^{:doc "First version of GPS (General Problem Solver)"}
paip.gps1)

(def state
  "The current state: a set of conditions."
  #{})

(def ops
  "A set of available operators."
  #{})

(def school-ops
  "An operation"
  #{{:action   'drive-son-to-school
     :preconds #{'son-at-home 'car-works}
     :add-set  #{'son-at-school}
     :del-set  #{'son-at-home}}})
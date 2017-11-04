(ns ^{:doc "Final version of GPS"}
paip.gps
  (:require [paip.gps1 :refer :all]))

(defn starts-with
  "Is this a list whose first element is x?"
  [list x]
  (and (not (atom? list))
       (= (first list) x)))





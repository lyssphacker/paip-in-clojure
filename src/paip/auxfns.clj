(ns ^{:doc "Auxiliary functions used by all other programs"}
paip.auxfns)

(defn mappend
  "Append the results of calling fn on each element of list."
  [fn list]
  (apply concat (map fn list)))
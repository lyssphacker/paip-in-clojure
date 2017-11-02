(ns paip.auxfns
  ^{:doc "Auxiliary functions used by all other programs"})

(defn mappend
  "Append the results of calling fn on each element of list."
  [fn list]
  (apply concat (map fn list)))
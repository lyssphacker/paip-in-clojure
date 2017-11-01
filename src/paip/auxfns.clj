(ns paip.auxfns)

(defn mappend
  "Append the results of calling fn on each element of list."
  [fn list]
  (apply concat (map fn list)))
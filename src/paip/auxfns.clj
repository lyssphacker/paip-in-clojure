(ns ^{:doc "Auxiliary functions used by all other programs"}
  paip.auxfns)

(defn mappend
  "Append the results of calling fn on each element of list."
  [fn list]
  (apply concat (map fn list)))

(defn contains-val?
  [coll val]
  (when (seq coll)
    (or (= val (first coll))
        (recur (next coll) val))))

(defn appropriate?
  "An op is appropriate to a goal if it is in its add list."
  [goal]
  (fn [op]
    (contains-val? (:add-vec op) goal)))

(defn find-all
  "Find all those elements of sequence that match item"
  [goal ops]
  (filter (appropriate? goal) ops))


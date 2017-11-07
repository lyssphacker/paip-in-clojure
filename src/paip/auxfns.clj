(ns ^{:doc "Auxiliary functions used by all other programs"}
  paip.auxfns)

(defn mappend
  "Append the results of calling fn on each element of list."
  [fn list]
  (apply concat (map fn list)))

(defn in?
  "true if coll contains elm"
  [elm coll]
  (some #(= elm %) coll))

(defn appropriate?
  "An op is appropriate to a goal if it is in its add list."
  [goal]
  (fn [op]
    (in? goal (:add-vec op))))

(defn find-all
  "Find all those elements of sequence that match item"
  [goal ops]
  (filter (appropriate? goal) ops))

(defn show-stacktrace
  []
  (clojure.stacktrace/print-stack-trace *e 10))


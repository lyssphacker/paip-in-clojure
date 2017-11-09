(ns paip.eliza1)

(defn variable?
  [x]
  (and (symbol? x)
       (= (get (name x) 0) \?)))
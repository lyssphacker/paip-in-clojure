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

(defmacro << [^String string]
  (let [-re #"#\{(.*?)\}"
        fstr (clojure.string/replace string -re "%s")
        fargs (map #(read-string (second %)) (re-seq -re string))]
    `(format ~fstr ~@fargs)))

;;;; PATTERN MATCHING FACILITY

(defn variable?
  "Is x a variable (a symbol beginning with `?')?"
  [x]
  (and (symbol? x)
       (= (get (name x) 0) \?)))

(def fail nil)

(def no-bindings {:true :true})

(defn match-variable
  [var input bindings]
  (cond
    (not (contains? bindings var)) (assoc bindings var input)
    (= input (var binding)) bindings
    :else fail))
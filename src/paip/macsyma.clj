(ns ^{:doc "The implementation of MACSYMA"}
paip.macsyma
  (:require [better-cond.core :as b]
            [paip.patmatch :refer (expand-pat-match-abbrev
                                    rule-based-translator
                                    pat-match)]
            [clojure.math.numeric-tower :as math]
            [clojure.inspector :refer (atom?)]
            [clojure.walk :refer (postwalk-replace)]))

(def infix->prefix-rules
  (map expand-pat-match-abbrev
       '(((x+ = y+) (= x y))
          ((- x+) (- x))
          ((+ x+) (+ x))
          ((x+ + y+) (+ x y))
          ((x+ - y+) (- x y))
          ((x+ * y+) (* x y))
          ((x+ / y+) (/ x y))
          ((x+ expt y+) (expt x y)))))

(defn rule-pat [rule] (first rule))
(defn rule-res [rule] (second rule))

(defn infix->prefix
  [exp]
  (b/cond
    (atom? exp) exp
    (= (count exp) 1) (infix->prefix (first exp))
    :when-let [res
               (rule-based-translator
                 exp
                 infix->prefix-rules
                 pat-match
                 rule-pat
                 rule-res
                 (fn
                   [bindings response]
                   (postwalk-replace
                     (map
                       (fn [pair]
                         (cons (first pair)
                               (infix->prefix (rest pair))))
                       bindings)
                     response)))] res
    (symbol? (first exp)) (list (first exp) (infix->prefix (rest exp)))
    :else (throw (Exception. "Illegal exp"))))


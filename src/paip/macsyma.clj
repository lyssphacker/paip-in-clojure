(ns ^{:doc "The implementation of MACSYMA"}
paip.macsyma
  (:require [better-cond.core :as b]
            [paip.patmatch :refer (expand-pat-match-abbrev
                                    rule-based-translator
                                    pat-match)]
            [clojure.math.numeric-tower :as math]))

(defn foo
  [x]
  (b/cond
    (= x 1) 1
    :let [y x]
    (= y 2) 2
    :else 3))

(defn bar
  [x]
  (b/cond
    (= x 1) 1
    :when-let [y (= x 2)]
    2
    :else 3))

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

;(rule-based-translator
;  words
;  student-rules
;  pat-match
;  rule-pat
;  rule-res
;  (fn
;    [bindings response]
;    (postwalk-replace
;      (translate-pair bindings)
;      response)))


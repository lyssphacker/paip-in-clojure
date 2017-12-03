(ns ^{:doc "The implementation of MACSYMA"}
paip.macsyma
  (:require [better-cond.core :as b]
            [paip.patmatch :refer :all]
            [clojure.math.numeric-tower :as math]
            [clojure.inspector :refer (atom?)]
            [clojure.walk :refer (postwalk-replace)]
            [paip.auxfns :refer (fmap-values member)]
            [paip.student :refer (prefix->infix
                                   binary-exp?
                                   rule-pat rule-res
                                   exp-args exp? mkexp
                                   exp-lhs exp-rhs exp-op)]))

(def macsyma-pat-match-abbrev-map
  "Map of pattern matching abbreviations to their expansions."
  {'x+ '(?+ x),
   'y+ '(?+ y)
   'n  '(?is n numberp)
   'm  '(?is m numberp)
   's  '(?is s not-numberp)})

(def infix->prefix-rules
  (map (expand-pat-match-abbrev macsyma-pat-match-abbrev-map)
       '(((x+ = y+) (= x y))
          ((- x+) (- x))
          ((+ x+) (+ x))
          ((x+ + y+) (+ x y))
          ((x+ - y+) (- x y))
          ((x+ * y+) (* x y))
          ((x+ / y+) (/ x y))
          ((x+ expt y+) (expt x y)))))

(defn infix->prefix
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  [exp]
  (b/cond
    (atom? exp) exp
    (= (count exp) 1) (infix->prefix (first exp))
    :let [res
          (rule-based-translator
            exp
            infix->prefix-rules
            pat-match
            rule-pat
            rule-res
            (fn
              [bindings response]
              (postwalk-replace
                (fmap-values bindings infix->prefix)
                response)))]
    (not (nil? res)) res
    (symbol? (first exp)) (list (first exp) (infix->prefix (rest exp)))
    :else (throw (Exception. "Illegal exp"))))

(defn variable?
  "Variables are the symbols M through Z."
  ;; put x,y,z first to find them a little faster
  [exp]
  (member
    '(x y z m n o p q r s t u v w)
    exp))
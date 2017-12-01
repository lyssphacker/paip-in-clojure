(ns ^{:doc "Contains experiments, ad-hoc tests, etc."}
paip.foo
  (:require [paip.student :refer (student)]))

;(student '(if the number of customers Tom gets is twice the square of
;                  20 % of the number of advertisements he runs \,
;                  and the number of advertisements is 45 \,
;                  then what is the number of customers Tom gets ?))

(defn foo
  []
  (student '(if a is equal to b)))

(foo)
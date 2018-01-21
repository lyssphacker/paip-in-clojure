(ns ^{:doc "Version 1 of the prolog compiler, including the destructive unification routines from Chapter 11."}
  paip.prologc1
  (:require [paip.prolog :refer :all]
            [paip.auxfns :refer (defrecord+)]))

(def unbound "Unbound")

(defrecord+ variable [name [binding unbound]])

(defn bound-var?
  [var]
  (not= (:binding var) unbound))

(defmacro deref-var
  "Follow pointers for bound variables."
  [exp]
  `(loop [exp# ~exp]
     (if (and (variable? exp#) (bound-var? exp#))
       (recur (:binding exp#))
       exp#)))
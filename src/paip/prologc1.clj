(ns ^{:doc "Version 1 of the prolog compiler, including the destructive unification routines from Chapter 11."}
  paip.prologc1
  (:require [paip.prolog :refer :all]
            [paip.auxfns :refer (find-item)]))

(def unbound "Unbound")

;; Contains variables in the form {name {:name name :binding binding}}
(def vars {})

(defn bound-var?
  [var]
  (not= (:binding var) unbound))

(defn var??
  "Is var of the form {:name name :binding binding}?"
  [var]
  (let [keys (keys var)]
    (and (= (count keys) 2)
         (not (nil? (find-item keys :name)))
         (not (nil? (find-item keys :binding))))))

(defmacro deref-var
  "Follow pointers for bound variables."
  [exp]
  `(loop [exp# ~exp]
     (if (and (var?? exp#) (bound-var? exp#))
       (recur (:binding exp#))
       exp#)))

(defn set-binding!
  "Set var's binding to value.  Always succeeds (returns true)."
  [v value]
  (alter-var-root
    #'vars
    (fn [old-vars]
      (assoc old-vars () {:name })))
  true)
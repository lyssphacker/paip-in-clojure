(ns paip.simple
  (:require [paip.auxfns :refer :all]))

(defn one-of [set]
  "Pick one element of set, and make a list of it"
  (list (random-elt set)))

(defn verb [] (one-of '(hit took saw like)))

(defn noun [] (one-of '(man ball woman table)))

(defn article [] (one-of '(the a)))

(defn noun-phrase [] (concat (article) (noun)))

(defn verb-phrase [] (concat (verb) (noun-phrase)))

(defn sentence [] (concat (noun-phrase) (verb-phrase)))

;;; ==============================

(defn adj [] (one-of '(big little blue green adiabatic)))

(defn adj* []
  (if (= (rand-int 2) 0)
    nil
    (concat (adj) (adj*))))

(defn prep [] (one-of '(to in by with on)))

(defn pp [] (concat (prep) (noun-phrase)))

(defn pp* []
  (if (random-elt '(true false))
    (concat (pp) (pp*))
    nil))

;;; ==============================

(def simple-grammar
  "A grammar for a trivial subset of English."
  {:sentence    [[:noun-phrase :verb-phrase]]
   :noun-phrase [[:Article :Noun]]
   :verb-phrase [[:Verb :noun-phrase]]
   :Article     ["the" "a"]
   :Noun        ["man" "ball" "woman" "table"]
   :Verb        ["hit" "took" "saw" "liked"]})

(def grammar simple-grammar)

;;; ==============================

(defn rewrites
  [key]
  (if (contains? grammar key)
    (key grammar)
    nil))

(defn generate
  [phrase]
  (cond
    (vector? phrase) (mappend generate phrase)
    (rewrites phrase) (generate (random-elt (rewrites phrase)))
    :else (vector phrase)))

;;; ==============================

(def bigger-grammar
  {:sentence    [[:noun-phrase :verb-phrase]]
   :noun-phrase [[:Article :Adj* :Noun :PP*] [:Name] [:Pronoun]]
   :verb-phrase [[:Verb :noun-phrase :PP*]]
   :PP*         [[] [:PP :PP*]]
   :Adj*        [[] [:Adj :Adj*]]
   :PP          [:Prep :noun-phrase]
   :Prep        ["to" "in" "by" "with" "on"]
   :Adj         ["big" "little" "blue" "green" "adiabatic"]
   :Article     ["the" "a"]
   :Name        ["Pat" "Kim" "Lee" "Terry" "Robin"]
   :Noun        ["man" "ball" "woman" "table"]
   :Verb        ["hit" "took" "saw" "liked"]
   :Pronoun     ["he" "she" "it" "these" "those" "that"]})

;;; ==============================

(defn generate-tree
  [phrase]
  (cond
    (vector? phrase) (map generate-tree phrase)
    (rewrites phrase) (cons phrase
                            (generate-tree
                              (random-elt (rewrites phrase))))
    :else (vector phrase)))

;;; ==============================

(defn combine-all
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  [xlist ylist]
  (mappend (fn [y]
             (map (fn [x] (concat x y)) xlist))
           ylist))

(defn generate-all
  "Generate a list of all possible expansions of this phrase."
  [phrase]
  (cond
    (and (seq? phrase) (empty? phrase)) (list '())
    (sequential? phrase) (combine-all (generate-all (first phrase))
                                      (generate-all (rest phrase)))
    (rewrites phrase) (mappend generate-all (rewrites phrase))
    :else (list (list phrase))))

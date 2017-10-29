(ns paip.simple)

(defn random-elt [choices]
  "Choose an element from a list at random."
  (nth choices (rand-int (count choices))))

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

(def ^:dynamic grammar
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammers."
  simple-grammar)




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

(defn sentence []  (concat (noun-phrase) (verb-phrase)))

;;; ==============================

(defn adj* []
  (if (= (rand-int 2) 0)
    nil
    (concat (adj) (adj*))))




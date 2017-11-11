(ns ^{:doc "Advanced version of Eliza.
Has more rules, and accepts input without parens."}
paip.eliza
  (:require [paip.eliza1 :refer :all]
            [clojure.string :refer (replace)]))

(defn read-line-no-punct
  "Read an input line, ignoring punctuation."
  []
  (read-string
    (str
      "("
      (clojure.string/replace
        (read-line)
        #"[-.,;:`!?#()\"]"
        " ")
      ")")))
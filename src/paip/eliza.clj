(ns ^{:doc "Advanced version of Eliza.
Has more rules, and accepts input without parens."}
paip.eliza
  (:require [paip.eliza1 :refer (use-eliza-rules)]
            [clojure.pprint :refer (cl-format pprint)]))

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

(defn print-with-spaces
  [list]
  (cl-format true "~{~a ~}~%" list))

(defn eliza
  "Respond to user input using pattern matching rules."
  []
  (loop []
    (println 'eliza>)
    (let [input (read-line-no-punct)]
      (if (= input '(quit))
        'done
        (do
          (print-with-spaces
            (flatten
              (use-eliza-rules input)))
          (recur))))))
(ns ^{:doc "Final version of GPS"}
paip.gps
  (:require [paip.gps1 :refer :all]
            [clojure.string :as str]))

(defn executing?
  "Is x of the form: (executing ...) ?"
  [x]
  (str/starts-with? x "executing"))




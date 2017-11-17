(ns ^{:doc "Section 6.4 GPS based on explicit search"}
paip.gps-srch
  (:require [clojure.set :refer (subset?)]
            [paip.auxfns :refer (member)]
            [paip.gps :refer (executing? make-block-ops action?)]
            [paip.search :refer (beam-search)]))

(defn applicable-ops
  "Return a list of all ops that are applicable now."
  [state ops]
  (filter
    (fn [op]
      (subset? state (:preconds op)))
    ops))

(defn gps-successors
  "Return a list of states reachable from this one using ops."
  [state ops]
  (map
    (fn [op]
      (concat
        (filter
          (complement
            (fn [x]
              (member (:del-vec op) x)))
          state)
        (:add-vec op)))
    (applicable-ops state ops)))

(declare count-if)

(defn search-gps
  "Search for a sequence of operators leading to goal."
  ([start goal ops]
   (search-gps start goal ops 10))
  ([start goal ops beam-width]
   (filter
     action?
     (beam-search
       (cons 'start start)
       (fn [state] (subset? state goal))
       gps-successors
       (fn [state]
         (+ (count-if
              action?
              state)
            (count-if
              (fn [con]
                (not (member state con)))
              goal)))
       beam-width))))
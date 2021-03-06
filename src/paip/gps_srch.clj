(ns ^{:doc "Section 6.4 GPS based on explicit search"}
paip.gps-srch
  (:require [clojure.set :refer (subset?)]
            [paip.auxfns :refer (member count-if)]
            [paip.gps :refer (executing? make-block-ops action? convert-op)]
            [paip.search :refer (beam-search)]))

(defn applicable-ops
  "Return a list of all ops that are applicable now."
  [state ops]
  (filter
    (fn [op]
      (subset? (set (:preconds op)) (set state)))
    ops))

(defn gps-successors
  "Return a list of states reachable from this one using ops."
  [ops]
  (fn [state]
    (map
      (fn [op]
        (concat
          (filter
            (complement
              (fn [x]
                (member (:del-vec op) x)))
            state)
          (:add-vec op)))
      (applicable-ops state ops))))

(defn search-gps
  "Search for a sequence of operators leading to goal."
  ([start goal ops]
   (search-gps start goal ops 10))
  ([start goal ops beam-width]
   (filter
     action?
     (beam-search
       (list (cons 'start start))
       (fn [state]
         (subset? (set goal) (set state)))
       (gps-successors ops)
       (fn [state]
         (+ (count-if
              action?
              state)
            (count-if
              (fn [con]
                (not (member state con)))
              goal)))
       beam-width))))
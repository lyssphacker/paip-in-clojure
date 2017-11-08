(ns ^{:doc "Final version of GPS"}
paip.gps
  (:require [clojure.inspector :refer (atom?)]
            [paip.gps1 :refer (school-ops)]
            [paip.auxfns :refer (find-all in? <<)]
            [clojure.string :refer (starts-with? split)]
            [clojure.set :refer (subset?)]))

(defn executing?
  "Does x's name starts with 'executing'?"
  [x]
  (starts-with? x "executing"))

(defn convert-op
  "Make op conform to the executing-op convention."
  [op]
  (if (some executing? (:add-vec op))
    op
    (update op :add-vec
            (fn [add-vec]
              (cons
                (symbol (str 'executing- (name (:action op))))
                add-vec)))))

(def converted-school-ops
  (map convert-op school-ops))

(declare achieve-all)

(defn apply-op
  "Return a new, transformed state if op is applicable."
  [state goal op goal-stack ops]
  (let [state2
        (achieve-all state (:preconds op)
                     (cons goal goal-stack) ops)]
    (when-not
      (empty? state2)
      (concat
        (filter
          (complement
            (fn [x]
              (in? x (:del-vec op))))
          state2)
        (:add-vec op)))))

(defn achieve
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  [state goal goal-stack ops]
  (cond
    (in? goal state) state
    (in? goal goal-stack) '()
    :else (some
            (fn [op]
              (apply-op state goal op goal-stack ops))
            (find-all goal ops))))

(defn achieve-all
  "Achieve each goal, and make sure they still hold at the end."
  [state goals goal-stack ops]
  (with-local-vars [current-state state]
    (if
      (and
        (every?
          (fn [g]
            (var-set current-state
                     (achieve @current-state g goal-stack ops)))
          goals)
        (subset?
          (set goals)
          (set @current-state)))
      @current-state)))

(defn gps
  "General Problem Solver: from state, achieve goals using ops."
  [state goals ops]
  (filter
    #(or
       (executing? %)
       (= 'start %))
    (achieve-all
      (cons 'start state)
      goals
      []
      ops)))

;;; ==============================

(def banana-ops
  #{{:action   'climb-on-chair
     :preconds ['chair-at-middle-room 'at-middle-room 'on-floor]
     :add-vec  ['at-bananas 'on-chair]
     :del-vec  ['at-middle-room 'on-floor]},
    {:action   'push-chair-from-door-to-middle-room
     :preconds ['chair-at-door 'at-door]
     :add-vec  ['chair-at-middle-room 'at-middle-room]
     :del-vec  ['chair-at-door 'at-door]},
    {:action   'walk-from-door-to-middle-room
     :preconds ['at-door 'on-floor]
     :add-vec  ['at-middle-room]
     :del-vec  ['at-door]},
    {:action   'grasp-bananas
     :preconds ['at-bananas 'empty-handed]
     :add-vec  ['has-bananas]
     :del-vec  ['empty-handed]},
    {:action   'drop-ball
     :preconds ['has-ball]
     :add-vec  ['empty-handed]
     :del-vec  ['has-ball]},
    {:action   'eat-bananas
     :preconds ['has-bananas]
     :add-vec  ['empty-handed 'not-hungry]
     :del-vec  ['has-bananas 'hungry]}})

(def converted-banana-ops
  (map convert-op banana-ops))

;;; ==============================

(defn make-maze-op
  [here there]
  {:action   (symbol (<< "move-from-#{here}-to-#{there}"))
   :preconds [(symbol (<< "at-#{here}"))]
   :add-vec  [(symbol (<< "at-#{there}"))]
   :del-vec  [(symbol (<< "at-#{here}"))]})

(defn make-maze-ops
  [pair]
  (list (make-maze-op (first pair) (last pair))
        (make-maze-op (last pair) (first pair))))

(def maze-ops
  (mapcat make-maze-ops
          '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
             (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
             (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(def converted-maze-ops
  (map convert-op maze-ops))

;;; ==============================

(defn destination
  "Find the Y in 'executing-move-from-X-to-Y"
  [action]
  (symbol
    (last
      (split
        (name action)
        #"-"))))

(defn find-path
  "Search a maze for a path from start to end."
  [start end ops]
  (let [results
        (gps
          [(symbol (<< "at-#{start}"))]
          [(symbol (<< "at-#{end}"))]
          ops)]
    (when-not (empty? results)
      (cons start
            (map destination
                 (filter
                   (fn [x]
                     (not= 'start x))
                   results))))))

;;; ==============================

(defn move-ons
  [a b c]
  (if (= b 'table)
    [(symbol (<< "#{a}-on-#{c}"))]
    [(symbol (<< "#{a}-on-#{c}")) (symbol (<< "space-on-#{b}"))]))

(defn move-op
  [a b c]
  {:action   (symbol (<< "move-#{a}-from-#{b}-to-#{c}"))
   :preconds [(symbol (<< "space-on-#{a}"))
              (symbol (<< "space-on-#{c}"))
              (symbol (<< "#{a}-on-#{b}"))]
   :add-vec  (move-ons a b c)
   :del-vec  (move-ons a c b)})

(defn make-block-ops
  [blocks]
  (with-local-vars [ops []]
    (doseq [a blocks]
      (doseq [b blocks]
        (when-not (= a b)
          (doseq [c blocks]
            (when-not (or (= c a) (= c b))
              (conj ops (move-op a 'table b))))
          (var-set ops (conj @ops (move-op a 'table b)))
          (var-set ops (conj @ops (move-op a b 'table))))))
    @ops))
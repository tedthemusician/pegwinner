(ns pegwinner.board
  (:require [clojure.set :as set])
  (:gen-class))

(def holes [{3 1, 5 2}
            {6 3, 8 4}
            {7 4, 9 5}
            {0 1, 5 4, 10 6, 12 7}
            {11 7, 13 8}
            {0 2, 3 4, 12 8, 14 9}
            {1 3, 8 7}
            {2 4, 9 8}
            {1 4, 6 7}
            {2 5, 7 8}
            {3 6, 12 11}
            {4 7, 13 12}
            {3 7, 5 8, 10 11, 14 13}
            {4 8, 11 12}
            {5 9, 12 13}])

(defn get-middle
  "What hole are we (un)jumping over?"
  [[from to]]
  ((holes from) to))

(defn unjump
  "Rewind a board by one move, from its current state to the state
  before a jump"
  [b [from to]]
  (let [middle (get-middle [from to])]
    (-> b
        (disj from)
        (conj middle)
        (conj to))))

(defn get-state [moves]
  "Make all the moves in the move seq"
  (let [[[first-from first-to] & next-moves] moves]
    (reduce unjump #{first-from} moves)))

(defn legal?
  "Can we unjump from `from` to `to` on board `b`?"
  [b [from to]]
  (let [middle (get-middle [from to])]
  (not (or (contains? b middle) (contains? b to)))))

(defn hole-legal-moves [b h]
  "Every legal move for a given hole"
  (let [tos (map first (holes h))]
    (map
      (fn [to] [h to])
      (filter #(legal? b [h %]) tos))))

(defn board-legal-moves
  "Every legal move on the board"
  [b] (mapcat (partial hole-legal-moves b) b))

(defn start-pos?
  "Is this a starting position, i.e. is only one hole empty?"
  [b] (= 14 (count b)))


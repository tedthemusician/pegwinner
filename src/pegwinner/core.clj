(ns pegwinner.core
  (:require [pegwinner.constants :as const]
            [pegwinner.show :as show])
  (:gen-class))

;           0
;         1    2
;      3    4    5
;    6    7    8    9
; 10   11   12   13   14

(defn set-pegged
  "Set the plugged state of a hole"
  [state b n]
  (assoc-in b [n :plugged] state))

(def remove-peg (partial set-pegged false))
(def insert-peg (partial set-pegged true))

(defn get-middle
  "What hole are we (un)jumping over?"
  [b from to]
  (get-in b [from :moves to]))

(defn plugged? [b n] (get-in b [n :plugged]))

(defn legal?
  "Can we unjump from `from` to `to` on board `b`?"
  [b from to]
  (let [middle (get-middle b from to)]
    (not (or (plugged? b middle) (plugged? b to)))))

(defn hole-legal-moves [b n]
  "Every legal move for a given hole"
  (let [moves (map first (get-in b [n :moves]))]
    (filter #(legal? b n %) moves)))

(defn pairify
  "Convert a `from` and a list of `tos` to a list of [`from` `to`] vectors.
  In other words, a cartesian product where the first set has only
  one element."
  [[from [& tos]]]
  (map (fn [to] [from to]) tos))

(defn board-legal-moves
  "Every legal move on the board"
  [b]
  (let [plugged (map first (filter (fn [[n v]] (:plugged v)) b))
        hole-move-pairs (map (fn [n] [n (hole-legal-moves b n)]) plugged)
        froms-with-tos (filter #(not (empty? (second %))) hole-move-pairs)]
    (mapcat pairify froms-with-tos)))

(defn unjump
  "Rewind a board by one move, from its current state to the state
  before a jump"
  [b from to]
  (let [middle (get-middle b from to)]
    (-> b
        (remove-peg from)
        (insert-peg middle)
        (insert-peg to))))

(defn start-pos?
  "Is this a starting position, i.e. is only one hole empty?"
  [b]
  (= 1 (count (filter :plugged (vals b)))))

(defn accum-move
  "Make a move and add that move to the list of previous moves"
  [b prev-moves from to]
  [(unjump b from to) (conj prev-moves [from to])])

(defn make-next-legal-moves [[b prev-moves]]
  "Rewind a board from its current state to all possible past states"
  (let [legal-moves (board-legal-moves b)]
    (if (empty? legal-moves)
      nil
      (map #(apply accum-move b prev-moves %) legal-moves))))

(make-next-legal-moves [filled-12 []])

(def filled-12 (insert-peg const/empty-board 12))
(def filled-12-5 (insert-peg filled-12 5))
(def full-board (reduce #(insert-peg %1 %2) const/empty-board (range 25)))
(def top-empty (remove-peg full-board 0))

(def first-iteration (make-legal-moves filled-12))
(map show/print-board first-iteration)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

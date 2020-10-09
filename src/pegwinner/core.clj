(ns pegwinner.core
  (:require [pegwinner.constants :as const]
            [pegwinner.inspect :as ins]
            [pegwinner.show :as show])
  (:gen-class))

;           0
;         1    2
;      3    4    5
;    6    7    8    9
; 10   11   12   13   14

(defrecord BoardState [board prev-moves])

(defn set-pegged
  "Set the plugged state of a hole"
  [state b n]
  (assoc-in b [n :plugged] state))

(def remove-peg (partial set-pegged false))
(def insert-peg (partial set-pegged true))

(defn unjump
  "Rewind a board by one move, from its current state to the state
  before a jump"
  [b from to]
  (let [middle (ins/get-middle b from to)]
    (-> b
        (remove-peg from)
        (insert-peg middle)
        (insert-peg to))))

(defn accum-move
  "Make a move and add that move to the list of previous moves"
  [{:keys [board prev-moves]} from to]
  (->BoardState (unjump board from to) (conj prev-moves [from to])))

(defn make-next-legal-moves
  "Rewind a board from its current state to all possible past states"
  [board-state]
  (let [{:keys [board prev-moves]} board-state
        legal-moves (ins/board-legal-moves board)]
    (if (and (not (ins/start-pos? board)) (empty? legal-moves))
      []
      (map #(apply accum-move board-state %) legal-moves))))

(def make-next-legal-moves-memo (memoize make-next-legal-moves))

(defn find-winning-moves
  "Find every sequence of moves that lead to these states"
  [board-states]
  (let [done? (ins/start-pos? (:board (first board-states)))]
    (if done?
      (map :prev-moves board-states)
      (recur (mapcat make-next-legal-moves-memo board-states)))))

(def filled-12 (insert-peg const/empty-board 12))
(def filled-12-5 (insert-peg filled-12 5))
(def full-board (reduce #(insert-peg %1 %2) const/empty-board (range 25)))
(def top-empty (remove-peg full-board 0))

(def bs (->BoardState filled-12 []))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

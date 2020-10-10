(ns pegwinner.core
  (:require [clojure.edn :as edn]
            [pegwinner.constants :as const]
            [pegwinner.inspect :as ins]
            [pegwinner.io :as io])
  (:gen-class))

; Our board is represented like this:
;           0
;         1    2
;      3    4    5
;    6    7    8    9
; 10   11   12   13   14

(defrecord BoardState [board prev-moves])
(defrecord WinningSequence [hole moves])

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

(defn get-winning-sequence
  "Convert a full board state into an empty hole and sequence of moves.
  Reverse the `from` and `to` of each move so it's played forward in time,
  then reverse the entire sequence so it's played forward in time."
  [{:keys [board prev-moves]}]
  (let [[[empty-hole]] (remove #(:plugged (second %)) board)
        reversed-moves (map reverse prev-moves)]
    (->WinningSequence empty-hole (reverse reversed-moves))))

(defn find-winning-moves
  "Find every sequence of moves that lead to these states"
  [board-states]
  (let [done? (ins/start-pos? (:board (first board-states)))]
    (if done?
      (map get-winning-sequence board-states)
      (recur (mapcat make-next-legal-moves-memo board-states)))))

(defn get-wins-by-end
  "Given the position of the last peg on the board, return:
  - The number of winning sequences
  - A sample sequence from each winnable starting position"
  [n]
  (let [won-board (insert-peg const/empty-board n)
        initial-board-state (->BoardState won-board [])
        winning-moves (find-winning-moves [initial-board-state])
        winning-moves-by-hole (group-by :hole winning-moves)]
    {:total (count winning-moves)
     :samples (map rand-nth (vals winning-moves-by-hole))}))

(defn -main
  "I do a whole lot now"
  [& args]
  (let [n (io/get-last-hole args)]
    (do
      (println (str "Finding ways to end with a peg in hole " n "."))
      (println "This may take several minutes...")
      (io/print-result n (get-wins-by-end (dec n))))))

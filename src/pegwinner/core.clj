(ns pegwinner.core
  (:require [pegwinner.board :as board]
            [pegwinner.io :as io])
  (:gen-class))

; Our board is represented like this:
;           1
;         2    3
;      4    5    6
;    7    8    9    10
; 11   12   13   14   15


(defn accum-move
  "Make a move and add that move to the list of previous moves"
  [{:keys [board plugged-hole prev-moves]} from to]
  (->BoardState (unjump board from to)
                plugged-hole
                (conj prev-moves [from to])))

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
  "Show a user how to win for a given end state"
  [& args]
  (let [n (io/get-last-hole args)]
    (do
      (println (str "Finding ways to end with a peg in hole " n "."))
      (println "This may take several minutes...")
      (io/print-result n (get-wins-by-end (dec n))))))

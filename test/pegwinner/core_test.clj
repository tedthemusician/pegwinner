(ns pegwinner.core-test
  (:require [clojure.test :refer :all]
            [pegwinner.core :refer :all]
            [pegwinner.bits :as bits]
            [pegwinner.board :as board]))

(defn def-moves [f]
  (def move-bit-pairs [[[3 5] 53]
                       [[11 4] -76]
                       [[5 0] 80]
                       [[14 12] -20]
                       [[1 8] 24]])
  (def moves-raw (map first move-bit-pairs))
  (def moves-bits (map second move-bit-pairs))
  (def packed-moves (take 13 (concat moves-bits (repeat (byte 0)))))
  (f))

(defn def-board-states [f]
  (def sample-boards [#{3 4 7 12 13}
                      #{0 5 10 12}
                      #{3}
                      #{1}])
  (def sample-board (first sample-boards))
  (f))

(defn def-valid-moves [f]
  (def valid-move-sequences
    [{:moves [[12 10]
              [11 13]
              [12 5]
              [13 11]
              [12 14]
              [8 6]
              [6 1]
              [7 9]
              [3 12]
              [1 6]
              [5 0]
              [3 5]
              [0 3]]
      :board-state (set (range 1 15))
      :legal-moves []}
     {:moves [[1 8]
              [8 6]
              [7 9]
              [9 2]
              [4 11]
              [6 1]
              [11 13]
              [12 10]
              [5 14]
              [3 5]
              [10 3]]
      :board-state #{1 2 3 4 5 6 7 8 9 11 13 14}
      :legal-moves []}
     {:moves [[3 10]
              [6 1]
              [10 12]
              [11 4]
              [12 10]]
      :board-state #{1 3 4 7 10 11}
      :legal-moves [[4 13]
                    [7 9]
                    [11 13]]}
     {:moves [[0 5]
              [5 3]
              [2 9]
              [4 11]
              [5 0]]
      :board-state #{0 2 3 7 9 11}
      :legal-moves [[3 5]
                   [3 10]
                   [11 13]]}])
  (def given-sequence (first valid-move-sequences))
  (f))

(use-fixtures :once def-moves def-board-states def-valid-moves)

(deftest get-nibbles
  (testing "Ints are correctly converted to nibbles"
    (is (= (bits/get-nibble 0) 0))
    (is (= (bits/get-nibble 1) 1))
    (is (= (bits/get-nibble 15) 15))
    (is (= (bits/get-nibble 16) 0))
    (is (= (bits/get-nibble 17) 1))
    (is (= (bits/get-nibble 31) 15))
    (is (= (bits/get-nibble 127) 15))
    (is (= (bits/get-nibble 128) 0))
    (is (= (bits/get-nibble 129) 1))
    (is (= (bits/get-nibble -1) 15))))

(deftest move-packing
  (testing "Moves are correctly packed"
    (is (= (map bits/pack-move moves-raw) moves-bits))))

(deftest move-unpacking
  (testing "Moves are correctly unpacked"
    (is (= (map bits/unpack-move moves-bits) moves-raw))))

(deftest move-packing-symmetry
  (testing "Packing and unpacking moves and vice versa is a no-op"
    (is (= moves-raw
           (map (comp bits/unpack-move bits/pack-move) moves-raw)))
    (is (= moves-bits
           (map (comp bits/pack-move bits/unpack-move) moves-bits)))))

(deftest moves-packing
  (testing "Seqs of unpacked moves are correctly packed"
    (is (= packed-moves (bits/pack-moves moves-raw)))))

(deftest moves-unpacking
  (testing "Seqs of packed moves are correctly unpacked"
    (is (= moves-raw (bits/unpack-moves moves-bits)))))

(deftest moves-packing-symmetry
  (testing "Packing and unpacking lists of moves and vice versa is a no-op"
    (is (= ((comp bits/unpack-moves bits/pack-moves) moves-raw) moves-raw))
    (is (= ((comp bits/pack-moves bits/unpack-moves) packed-moves) packed-moves))))



(deftest getting-middles
  (testing "Providing a from and to to get-middle returns the middle"
    (is (= (board/get-middle [3 0]) 1))
    (is (= (board/get-middle [7 2]) 4))
    (is (= (board/get-middle [8 1]) 4))
    (is (= (board/get-middle [6 8]) 7))
    (is (= (board/get-middle [4 11]) 7))
    (is (= (board/get-middle [2 9]) 5))))

(deftest unjumping
  (testing "Unjumping over a hole returns a correctly-updated board state"
    (is (= (board/unjump sample-board [3 0]) #{0 1 4 7 12 13}))
    (is (= (board/unjump sample-board [3 10]) #{4 6 7 10 12 13}))
    (is (= (board/unjump sample-board [7 9]) #{3 4 8 9 12 13}))
    (is (= (board/unjump sample-board [12 5]) #{3 4 5 7 8 13}))
    (is (= (board/unjump sample-board [12 10]) #{3 4 7 10 11 13}))))

(deftest moves-to-state
  (testing "A list of moves is correctly reduced to a board state"
    (let [pairs (map
                  (juxt (comp board/get-state :moves) :board-state)
                  valid-move-sequences)]
      (is (= (first (first pairs))  (second (first pairs))))
      (is (= (first (second pairs)) (second (second pairs))))
      (is (= (first (nth pairs 2))  (second (nth pairs 2))))
      (is (= (first (nth pairs 3))  (second (nth pairs 3)))))))

(deftest move-legality
  (testing "Legal moves return true"
    (is (board/legal? sample-board [3 0]))
    (is (board/legal? sample-board [3 10]))
    (is (board/legal? sample-board [7 9]))
    (is (board/legal? sample-board [12 5]))
    (is (board/legal? sample-board [12 10])))
  (testing "Moves from an unplugged hole return false"
    (is (not (board/legal? sample-board [5 14])))
    (is (not (board/legal? sample-board [5 0])))
    (is (not (board/legal? sample-board [0 3]))))
  (testing "Moves to a plugged hole return false"
    (is (not (board/legal? sample-board [13 4])))
    (is (not (board/legal? sample-board [9 7]))))
  (testing "Moves over a plugged hole return false"
    (is (not (board/legal? sample-board [13 11])))
    (is (not (board/legal? sample-board [4 11])))
    (is (not (board/legal? sample-board [3 5])))))

(deftest hole-legality
  (testing "A hole with legal moves returns exactly those moves"
    (is (= (set (board/hole-legal-moves sample-board 3))
           (set [[3 0] [3 10]])))
    (is (= (set (board/hole-legal-moves sample-board 7))
           (set [[7 9]])))
    (is (= (set (board/hole-legal-moves sample-board 12))
           (set [[12 10] [12 5]]))))
  (testing "A hole with no legal moves returns an empty seq"
    (is (= (board/hole-legal-moves sample-board 4) []))
    (is (= (board/hole-legal-moves sample-board 13) []))
    )
  (testing "An empty hole returns an empty seq"
    (is (= (board/hole-legal-moves sample-board 0) []))
    (is (= (board/hole-legal-moves sample-board 1) []))
    (is (= (board/hole-legal-moves sample-board 2) []))
    (is (= (board/hole-legal-moves sample-board 5) []))
    (is (= (board/hole-legal-moves sample-board 6) []))
    (is (= (board/hole-legal-moves sample-board 8) []))
    (is (= (board/hole-legal-moves sample-board 9) []))
    (is (= (board/hole-legal-moves sample-board 10) []))
    (is (= (board/hole-legal-moves sample-board 11) []))
    (is (= (board/hole-legal-moves sample-board 14) []))))

(deftest board-legality
  (testing "A board with legal moves returns exactly those legal moves"
    (let [pairs (map
                  (juxt
                    (comp set board/board-legal-moves :moves)
                    (comp set :legal-moves))
                  valid-move-sequences)]
      (is (= (first (first pairs))  (second (first pairs))))
      (is (= (first (second pairs)) (second (second pairs))))
      (is (= (first (nth pairs 2))  (second (nth pairs 2))))
      (is (= (first (nth pairs 3))  (second (nth pairs 3)))))))

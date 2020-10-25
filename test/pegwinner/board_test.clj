(ns pegwinner.board-test
  (:require [clojure.test :refer :all]
            [pegwinner.board :as board]))

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

(use-fixtures :once def-board-states def-valid-moves)

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





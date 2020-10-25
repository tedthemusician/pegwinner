(ns pegwinner.bits-test
  (:require [clojure.test :refer :all]
            [pegwinner.bits :as bits]))

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

(use-fixtures :once def-moves)

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


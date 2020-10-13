(ns pegwinner.bits
  (:require [pegwinner.structures :refer :all])
  (:gen-class))

(deftype Board [^byte high ^byte low])

(defn show-byte [b] (Integer/toBinaryString (bit-and 0xff b)))

(defn- pack-board
  "Store a board state (a seq of plugged holes) in 16 bits.
  The first bit is unused; the other 15 are split among two bytes. Each bit
  represents the plugged state of a hole."
  [b]
  (let [individual-bits (map #(bit-shift-left 1 (dec %)) b)
        merged-bits (reduce bit-or 0 individual-bits)
        low-byte (unchecked-byte merged-bits)
        high-byte (unsigned-bit-shift-right merged-bits 8)]
    (->Board high-byte low-byte)))

(defn- unpack-board
  "Extract a board state (a seq of plugged holes) from a high and low byte
  where each bit represents the plugged state of a hole"
  [high-raw low-raw]
  (let [high (bit-shift-left (bit-and 0xff high-raw) 8)
        low (bit-and 0xff low-raw)
        all (bit-or low high)]
    (keep-indexed #(if (bit-test all %2) (inc %1)) (range 15))))

(defn- get-nibble
  "Get the four lowest bits of a number"
  [n] (bit-and 2r1111 n))

(defn- pack-move
  "Store a `from` and `to` into a pair of nibbles"
  [{:keys [from to]}]
  (let [from-bits (bit-shift-left from 4)]
    (unchecked-byte (+ from-bits to))))

(defn- unpack-move
  "Extract the `from` and `to` nibbles from a move byte"
  [b]
  (let [to (get-nibble b)
        from (get-nibble (unsigned-bit-shift-right b 4))]
    (->Move from to)))

(defn- pack-moves
  "Pack each move into a byte; pad right with 0s"
  [moves]
  (take 13 (concat (map pack-move moves) (repeat (byte 0)))))

(defn- unpack-moves
  "Unpack a set of bytes into moves and discard 0 padding"
  [move-bytes-padded]
  (let [move-bytes (take-while (comp not zero?) move-bytes-padded)]
    (map unpack-move move-bytes)))

(defn pack-board-state
  "Create a 16-byte array that represents a board state:
  - 2 bytes for current board position
  - 1 byte for initially-plugged hole
  - 13 bytes for moves made"
  [{:keys [board plugged-hole moves]}]
  (let [packed-board (pack-board board)
        board-high-byte (.high packed-board)
        board-low-byte (.low packed-board)
        packed-moves (map pack-move moves)
        move-chunk (take 13 (concat packed-moves (repeat (byte 0))))
        all-bytes (concat [board-high-byte
                           board-low-byte
                           (byte plugged-hole)]
                          move-chunk)]
    (byte-array all-bytes)))

(defn unpack-board-state
  "Extract current position, initially-plugged hole, and list of moves
  from packed board"
  [byte-arr]
  (let [[board-high board-low hole-byte & move-bytes] (vec byte-arr)
        board (unpack-board board-high board-low)
        moves (unpack-moves move-bytes)]
    (->BoardState board hole-byte moves)))

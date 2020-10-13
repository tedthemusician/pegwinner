(ns pegwinner.bits
  (:gen-class))

(defn show-byte [b] (Integer/toBinaryString (bit-and 0xff b)))

(defn- get-nibble
  "Get the four lowest bits of a number"
  [n] (bit-and 2r1111 n))

(defn- pack-move
  "Store a `from` and `to` into a pair of nibbles"
  [[from to]]
  (let [from-bits (bit-shift-left from 4)]
    (unchecked-byte (+ from-bits to))))

(defn- unpack-move
  "Extract the `from` and `to` nibbles from a move byte"
  [packed-move]
  [(get-nibble packed-move)
   get-nibble (unsigned-bit-shift-right packed-move 4)])

(defn pack-moves
  "Pack each move into a byte; pad right with 0s"
  [moves]
  (take 13 (concat (map pack-move moves) (repeat (byte 0)))))

(defn unpack-moves
  "Unpack a set of bytes into moves and discard 0 padding"
  [move-bytes-padded]
  (let [move-bytes (take-while (comp not zero?) move-bytes-padded)]
    (map unpack-move move-bytes)))

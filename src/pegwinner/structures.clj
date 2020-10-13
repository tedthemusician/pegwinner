(ns pegwinner.structures
  (:gen-class))

(defrecord BoardState [board plugged-hole moves])

(defrecord WinningSequence [hole moves])

(defrecord Move [from to])


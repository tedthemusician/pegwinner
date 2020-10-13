(ns pegwinner.constants
  (:require [clojure.string :as str])
  (:gen-class))

(def hole-groups [[1] [2 3] [4 5 6] [7 8 9 10] [11 12 13 14 15]])

(def holes [{4 2, 6 3}
            {7 4, 9 5}
            {8 5, 10 6}
            {1 2, 6 5, 11 7, 13 8}
            {12 8, 14 9}
            {1 3, 4 5, 13 9, 15 10}
            {2 4, 9 8}
            {3 5, 10 9}
            {2 5, 7 8}
            {3 6, 8 9}
            {4 7, 13 12}
            {5 8, 14 13}
            {4 8, 6 9, 11 12, 15 14}
            {5 9, 12 13}
            {6 10, 13 14}])

(defn initialize [h] {:moves h, :plugged false})

(def empty-board
  (into (sorted-map)
        (map-indexed vector
                     (map initialize holes))))

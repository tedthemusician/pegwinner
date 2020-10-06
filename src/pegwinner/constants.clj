(ns pegwinner.constants
  (:require [clojure.string :as str])
  (:gen-class))

(def hole-groups [[0] [1 2] [3 4 5] [6 7 8 9] [10 11 12 13 14]])

(def holes [{3 1, 5 2}
            {6 3, 8 4}
            {7 4, 9 5}
            {0 1, 5 4, 10 6, 12 7}
            {11 7, 13 8}
            {0 2, 3 4, 12 8, 14 9}
            {1 3, 8 7}
            {2 4, 9 8}
            {1 4, 6 7}
            {2 5, 7 8}
            {3 6, 12 11}
            {4 7, 13 12}
            {3 7, 5 8, 10 11, 14 13}
            {4 8, 11 12}
            {5 9, 12 13}])

(def empty-board
  (into (sorted-map)
        (map-indexed vector
                     (map
                       #(assoc % :plugged false)
                       holes))))


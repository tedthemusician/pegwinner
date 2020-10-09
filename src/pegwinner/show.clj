(ns pegwinner.show
  (:require [clojure.string :as str])
  (:require [pegwinner.constants :as const])
  (:gen-class))

(defn get-row
  "Get holes `is` from board `b`"
  [b is]
  (map vector is (map (partial get b) is)))

(defn render-hole
  "If plugged, show the hole's index; otherwise show a dot"
  [[i h]]
  (if (:plugged h) (format "%2d " i) " . "))

(defn render-row
  "Show indices or dots for a row. Indent the row as needed to show the board
  as a triangle"
  [r]
  (let [num-holes (count r)
        padding (str/join (replicate (- 5 num-holes) "  "))
        repr (str/join " " (map render-hole r))]
    (str padding repr))) 

(defn render-board
  "Render a board as indices and dots in a triangle"
  [b]
  (let [rows (map (partial get-row b) const/hole-groups)]
    (str/join "\n" (map render-row rows))))

(defn print-board
  "Print a board as indices and dots in a triangle"
  [b] (println (render-board b)))

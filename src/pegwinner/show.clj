(ns pegwinner.show
  (:require [clojure.string :as str])
  (:require [pegwinner.constants :as const])
  (:gen-class))

(defn get-row [b is] (map vector is (map (partial get b) is)))

(defn render-hole [[i h]] (if (:plugged h) (format "%2d " i) " . "))

(defn render-row [r]
  (let [num-holes (count r)
        padding (str/join (replicate (- 5 num-holes) "  "))
        repr (str/join " " (map render-hole r))]
    (str padding repr))) 

(defn render-board [b]
  (let [rows (map (partial get-row b) const/hole-groups)]
    (str/join "\n" (map render-row rows))))

(defn print-board [b] (println (render-board b)))

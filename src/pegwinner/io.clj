(ns pegwinner.io
  (:require [clojure.string :as str])
  (:require [clojure.edn :as edn])
  (:require [pegwinner.constants :as const])
  (:gen-class))

(defn- get-valid-input
  "Make sure the user gave us a valid hole number (1-15)"
  [input]
  (let [n (try (edn/read-string input)
               (catch Exception e false))]
    (if (and (= java.lang.Long (type n))
          (>= n 1)
          (<= n 15))
      n
      nil)))

(defn- prompt-input []
  (do (print "Which hole should have a peg [1-15]? ")
      (flush)
      (read-line)))

(defn get-last-hole
  "Request the last hole from the user if it was not provided as an argument
  or if the argument is invalid"
  [[arg]]
  (let [input (or arg (prompt-input))]
    (if-let [n (get-valid-input input)]
      n
      (do (println
            (if arg
              "The argument for the last hole must be a number between 1 and 15."
              "Please enter a number between 1 and 15."))
          (get-last-hole)))))

(defn- get-row
  "Get holes `is` from board `b`"
  [b is]
  (map vector is (map (partial get b) is)))

(defn- render-hole
  "If plugged, show the hole's index; otherwise show a dot"
  [[i h]]
  (if (:plugged h) (format "%2d " i) " . "))

(defn- render-row
  "Show indices or dots for a row. Indent the row as needed to show the whole
  board as a triangle"
  [r]
  (let [num-holes (count r)
        padding (str/join (replicate (- 5 num-holes) "  "))
        repr (str/join " " (map render-hole r))]
    (str padding repr))) 

(defn- render-board
  "Render a board as indices and dots in a triangle"
  [b]
  (let [rows (map (partial get-row b) const/hole-groups)]
    (str/join "\n" (map render-row rows))))

(def print-board (comp println render-board))

(defn- render-winning-sequence
  "Render a winning sequence as an empty hole and sequence of moves.
  Index holes by 1."
  [{:keys [hole moves]}]
  (let [shown-moves (map
                      #(str (first %) "->" (second %))
                      (map (partial map inc) moves))]
    (str "Starting hole: " (inc hole) "\n" (str/join "\n" shown-moves) "\n")))

(defn- print-total
  "Show the user how many ways there are to reach this ending"
  [n total]
  (println (str
             "Number of solutions that end on hole "
             n
             ": "
             total)))

(defn- print-winning-sequences
  "Show the user how to win for given starting positions"
  [sequences]
  (println (str/join "\n" (map render-winning-sequence sequences))))

(defn print-result
  "Show the user how many wins are possible and how to win for each winnable
  starting position"
  [n {:keys [total samples]}]
  (do
    (print-total n total)
    (println "Sample solutions for each winnable starting position:")
    (print-winning-sequences samples)))

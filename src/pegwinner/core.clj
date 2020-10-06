(ns pegwinner.core
  (:require [clojure.pprint :as pp]
            [pegwinner.constants :as const]
            [pegwinner.show :as show])
  (:gen-class))

;           0
;         1    2
;      3    4    5
;    6    7    8    9
; 10   11   12   13   14

(defn set-pegged [state b n]
  (assoc-in b [n :plugged] state))

(def remove-peg (partial set-pegged false))
(def insert-peg (partial set-pegged true))

(defn get-middle [b from to] (get-in b [from to]))

(defn plugged? [b n] (get-in b [n :plugged]))

(defn legal? [b from to]
  (let [middle (get-middle b from to)]
    (not (or (plugged? b middle) (plugged? b to)))))

(defn unjump [b from to]
  (let [middle (get-middle b from to)]
    (-> b
        (remove-peg from)
        (insert-peg middle)
        (insert-peg to))))

(def filled-12 (insert-peg const/empty-board 12))

(def filled-board (reduce #(insert-peg %1 %2) const/empty-board (range 15)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

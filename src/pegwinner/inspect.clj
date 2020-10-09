(ns pegwinner.inspect
  (:gen-class))

(defn get-middle
  "What hole are we (un)jumping over?"
  [b from to]
  (get-in b [from :moves to]))

(defn plugged? [b n] (get-in b [n :plugged]))

(defn legal?
  "Can we unjump from `from` to `to` on board `b`?"
  [b from to]
  (let [middle (get-middle b from to)]
    (not (or (plugged? b middle) (plugged? b to)))))

(defn hole-legal-moves [b n]
  "Every legal move for a given hole"
  (let [moves (map first (get-in b [n :moves]))]
    (filter #(legal? b n %) moves)))

(defn pairify
  "Convert a `from` and a list of `tos` to a list of [`from` `to`] vectors.
  In other words, a cartesian product where the first set has only
  one element."
  [[from [& tos]]]
  (map (fn [to] [from to]) tos))

(defn board-legal-moves
  "Every legal move on the board"
  [b]
  (let [plugged (map first (filter (fn [[n v]] (:plugged v)) b))
        hole-move-pairs (map (fn [n] [n (hole-legal-moves b n)]) plugged)
        froms-with-tos (filter #(not (empty? (second %))) hole-move-pairs)]
    (mapcat pairify froms-with-tos)))

(defn start-pos?
  "Is this a starting position, i.e. is only one hole empty?"
  [b]
  (= 1 (count (filter :plugged (vals b)))))


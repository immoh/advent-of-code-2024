(ns day04
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (into {}
        (map-indexed (fn [i row]
                       [i (into {} (map-indexed vector row))])
                     (str/split-lines input))))

(defn cols [board]
  (count (get board 0)))

(defn rows [board]
  (count board))

(defn word [board start [dx dy] length]
  (->> (iterate (fn [[x y]]
                  [(+ x dx) (+ y dy)])
                start)
       (take length)
       (map (partial get-in board))
       (apply str)))

(defn find-words-of-length [max-length board]
  (for [x  (range (rows board))
        y  (range (cols board))
        dx [-1 0 1]
        dy [-1 0 1]
        :when (not= dx dy 0)]
    (word board [x y] [dx dy] max-length)))

;; Part 1

(defn part1 [input]
  (->> input
       (parse-input)
       (find-words-of-length 4)
       (filter #(= "XMAS" %))
       (count)))

;; Part 2

(defn x-shape-words [board [x y]]
  [(word board [(dec x) (dec y)] [1 1] 3)
   (word board [(inc x) (dec y)] [-1 1] 3)])

(defn find-x-shapes [board]
  (for [x (range (rows board))
        y (range (cols board))]
    (x-shape-words board [x y])))

(defn part2 [input]
  (->> input
       (parse-input)
       (find-x-shapes)
       (filter (partial every? #{"SAM" "MAS"}))
       (count)))

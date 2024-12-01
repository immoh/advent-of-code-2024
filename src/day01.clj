(ns day01
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (let [pairs (map (fn [line]
                     (map parse-long (re-seq #"\d+" line)))
                   (str/split-lines input))]
    [(map first pairs) (map second pairs)]))

;; Part 1

(defn distance [a b]
  (abs (- b a)))

(defn part1 [input]
  (->> input
       (parse-input)
       (map sort)
       (apply map distance)
       (reduce +)))

;; Part 2

(defn similarity-score [list1 list2]
  (let [list2-frequencies (frequencies list2)]
    (->> list1
         (map #(list2-frequencies % 0))
         (map * list1)
         (reduce +))))

(defn part2 [input]
  (->> input
       (parse-input)
       (apply similarity-score)))

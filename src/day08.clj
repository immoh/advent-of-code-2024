(ns day08
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    {:rows     (count lines)
     :cols     (count (first lines))
     :antennas (for [[i line] (map-indexed vector lines)
                     [j c] (map-indexed vector line)
                     :when (not= \. c)]
                 {:frequency c :pos [i j]})}))

(defn in-grid? [{:keys [rows cols]} [x y]]
  (and (<= 0 x (dec rows))
       (<= 0 y (dec cols))))

(defn antinodes [antinodes-of-pair-fn {:keys [antennas] :as city}]
  (->> (for [antenna1 antennas
             antenna2 antennas
             :when (and (= (:frequency antenna1) (:frequency antenna2))
                        (not= antenna1 antenna2))]
         [antenna1 antenna2])
       (mapcat (partial antinodes-of-pair-fn city))
       (set)))

;; Part 1

(defn antinodes-of-pair [city [{[x1 y1] :pos} {[x2 y2] :pos}]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (filter (partial in-grid? city) [[(- x1 dx) (- y1 dy)]
                                     [(+ x2 dx) (+ y2 dy)]])))

(defn part1 [input]
  (->> input
       (parse-input)
       (antinodes antinodes-of-pair)
       (count)))

;; Part 2

(defn antinodes-of-pair-2 [city [{[x1 y1] :pos} {[x2 y2] :pos}]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (concat
      (take-while (partial in-grid? city) (iterate (fn [[x y]] [(- x dx) (- y dy)]) [x1 y1]))
      (take-while (partial in-grid? city) (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) [x2 y2])))))

(defn part2 [input]
  (->> input
       (parse-input)
       (antinodes antinodes-of-pair-2)
       (count)))


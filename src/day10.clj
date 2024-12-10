(ns day10
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (reduce
    (fn [m [height pos]]
      (update m height (fnil conj #{}) pos))
    {}
    (for [[i line] (map-indexed vector (str/split-lines input))
          [j c] (map-indexed vector line)
          :let [height (parse-long (str c))]]
      [height [i j]])))

;; Part 1

(defn neighbors [pos]
  (map (partial mapv + pos) [[-1 0] [1 0] [0 -1] [0 1]]))

(defn score [topo-map trailhead]
  (count (reduce
           (fn [positions height]
             (->> positions
                  (mapcat neighbors)
                  (set)
                  (filter (get topo-map height))))
           [trailhead]
           (range 1 10))))

(defn part1 [input]
  (let [topo-map (parse-input input)]
    (->> (get topo-map 0)
         (map (partial score topo-map))
         (reduce +))))

;; Part 2

(defn rating [topo-map trailhead]
  (count (reduce
           (fn [positions height]
             (->> positions
                  (mapcat neighbors)
                  (filter (get topo-map height))))
           [trailhead]
           (range 1 10))))

(defn part2 [input]
  (let [topo-map (parse-input input)]
    (->> (get topo-map 0)
         (map (partial rating topo-map))
         (reduce +))))

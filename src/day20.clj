(ns day20
  (:require
    [clojure.string :as str]))

(defn parse-maze [input]
  (into {}
        (for [[i line] (map-indexed vector (str/split-lines input))
              [j tile] (map-indexed vector line)]
          [[i j] tile])))

(defn parse-input [input]
  (let [maze (group-by second (parse-maze input))
        start (ffirst (maze \S))
        end (ffirst (maze \E))]
    {:empty-tiles (set (concat (map first (maze \.)) [start end]))
     :walls       (set (map first (maze \#)))
     :start       start
     :end         end}))

(defn neighbors [empty-tiles pos]
  (->> [[0 1] [-1 0] [0 -1] [1 0]]
       (map #(mapv + pos %))
       (filter empty-tiles)))

(defn find-shortest-path [empty-tiles start end]
  (loop [wip {start [start]}
         visited #{}]
    (when-let [[pos path] (first (sort-by val wip))]
      (if (= end pos)
        path
        (recur (merge-with (fn [path1 path2]
                             (if (<= (count path1) (count path2))
                               path1
                               path2))
                           (dissoc wip pos)
                           (->> (neighbors empty-tiles pos)
                                (remove visited)
                                (map (fn [pos]
                                       [pos (conj path pos)]))
                                (into {})))
               (conj visited pos))))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn cheat-savings [best-path cheat-max-length]
  (->> (for [i (range (dec (count best-path)))
             j (range (inc i) (count best-path))
             :let [cheat-distance (manhattan-distance (nth best-path i) (nth best-path j))]
             :when (<= cheat-distance cheat-max-length)]
         (- j i cheat-distance))))

;; Part 1

(defn part1 [input]
  (let [{:keys [empty-tiles start end]} (parse-input input)
        best-path (find-shortest-path empty-tiles start end)]
    (->> (cheat-savings best-path 2)
         (filter (partial <= 100))
         (count))))

;; Part 2

(defn part2 [input]
  (let [{:keys [empty-tiles start end]} (parse-input input)
        best-path (find-shortest-path empty-tiles start end)]
    (->> (cheat-savings best-path 20)
         (filter (partial <= 100))
         (count))))

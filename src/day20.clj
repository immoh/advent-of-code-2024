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
  (loop [wip {start 0}
         visited #{start}]
    (when-let [[pos distance] (first (sort-by val wip))]
      (if (= end pos)
        distance
        (recur (merge-with min
                           (dissoc wip pos)
                           (zipmap (remove visited (neighbors empty-tiles pos))
                                   (repeat (inc distance))))
               (conj visited pos))))))

(defn part1 [input]
  (let [{:keys [empty-tiles walls start end]} (parse-input input)
        best-time (find-shortest-path empty-tiles start end)]
    (->> walls
         (keep #(find-shortest-path (conj empty-tiles %) start end))
         (map (fn [cheat-time] (- best-time cheat-time)))
         (filter #(>= % 100))
         (count))))

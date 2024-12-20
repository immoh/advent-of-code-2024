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
     :walls (set (map first (maze \#)))
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
         (keep-indexed (fn [i cheat-pos]
                         (do
                           (println i)
                           (find-shortest-path (conj empty-tiles cheat-pos) start end))))
         (map (fn [cheat-time] (- best-time cheat-time)))
         (filter #(>= % 100))
         (count))))

;; Part 2

(defn neighbors* [allowed pos]
  (->> [[0 1] [-1 0] [0 -1] [1 0]]
       (map #(mapv + pos %))
       (filter allowed)))

(defn cheat-paths* [empty-tiles walls max-length path]
  #_(let [all-paths (concat
                     (->> (last path)
                          (neighbors* empty-tiles)
                          (map (fn [end]
                                 {:start (first path) :end end :length (inc (count path))})))
                     (when (> (- max-length (count path)) 1)
                       (->> (last path)
                            (neighbors* walls)
                            (remove (set path))
                            (mapcat #(cheat-paths* empty-tiles walls (dec max-length) (conj path %))))))]
      (->> all-paths
           (group-by #(select-keys % [:start :end]))
           (vals)
           (map (comp first (partial sort-by :lengrh)))))
  (distinct (concat
             (->> (last path)
                  (neighbors* empty-tiles)
                  (remove (set path))
                  (map (fn [end]
                         {:start (first path) :first-wall (second path) :end end :length (inc (count path))})))
             (when (pos? (- max-length (count path)))
               (->> (last path)
                    (neighbors* walls)
                    (remove (set path))
                    (mapcat #(cheat-paths* empty-tiles walls (dec max-length) (conj path %))))))))

(defn cheat-paths [empty-tiles walls max-length]
  (mapcat (partial cheat-paths* empty-tiles walls max-length)
          (for [wall   walls
                start (neighbors* empty-tiles wall)]
            [start wall])))

(def memoized-find-shortest-path (memoize find-shortest-path))

(defn find-shortest-path-using-cheat [empty-tiles start end {cheat-start :start cheat-end :end cheat-length :length}]
  (when-let [first-half (memoized-find-shortest-path (conj empty-tiles cheat-start)
                                                     start
                                                     cheat-start)]
    (when-let [second-half (memoized-find-shortest-path empty-tiles
                                                        cheat-end
                                                        end)]
      (+ first-half second-half cheat-length -1))))


;; 65213 too low
;; 199185 too low
(defn part2 [input]
  (let [{:keys [empty-tiles walls start end]} (parse-input input)
        best-time (memoized-find-shortest-path empty-tiles start end)]
    (prn :best-time best-time)
    (->> (cheat-paths empty-tiles walls 20)
         (keep-indexed (fn [i cheat-path]
                         (do
                           (when (zero? (mod i 1000)) (println i))
                           (find-shortest-path-using-cheat empty-tiles start end cheat-path))))
         (map (fn [cheat-time] (- best-time cheat-time)))
         (filter #(>= % 50))
         (frequencies)
         (sort)
         #_(count))))



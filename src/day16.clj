(ns day16
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
     :start       start
     :end         end}))

(defn neighbors [empty-tiles {:keys [pos dir]}]
  (->> [[0 1] [-1 0] [0 -1] [1 0]]
       (remove #{(mapv (partial * -1) dir)})
       (keep (fn [new-dir]
              (let [new-pos (mapv + pos new-dir)]
                (when (empty-tiles new-pos)
                  [{:pos new-pos :dir new-dir}
                   (inc (if (= dir new-dir) 0 1000))]))))
       (into {})))

(defn find-shortest-path [empty-tiles start-state end-pred]
  (loop [wip {start-state 0}
         visited #{start-state}]
    (when-let [[current-state score] (first (sort-by val wip))]
      (if (end-pred current-state)
        {:state current-state :score score}
        (recur (merge-with min
                           (dissoc wip current-state)
                           (->> (neighbors empty-tiles current-state)
                                (remove (comp visited key))
                                (map (fn [[k new-score]]
                                       [k (+ score new-score)]))
                                (into {})))
               (conj visited current-state))))))

(defn part1 [input]
  (let [{:keys [empty-tiles start end]} (parse-input input)]
    (:score (find-shortest-path empty-tiles
                                {:pos start :dir [0 1]}
                                #(= end (:pos %))))))

;; Part 2

(defn on-best-path? [empty-tiles start end best-score empty-pos]
  (let [first-half (find-shortest-path empty-tiles
                                      {:pos start :dir [0 1]}
                                      #(= empty-pos (:pos %)))]
    (when (and first-half (<= (:score first-half) best-score))
      (when-let [second-half (find-shortest-path empty-tiles
                                                 (:state first-half)
                                                 #(= end (:pos %)))]
        (= best-score (reduce + (map :score [first-half second-half])))))))

;; Very slow
(defn part2 [input]
  (let [{:keys [empty-tiles start end]} (parse-input input)
        best-score (part1 input)]
    (->> empty-tiles
         (filter (partial on-best-path? empty-tiles start end best-score))
         (count))))

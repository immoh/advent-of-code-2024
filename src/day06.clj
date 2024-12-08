(ns day06
  (:require
   [clojure.string :as str]))

(defn parse-board [input]
  (into {}
        (map-indexed (fn [i row]
                       [i (into {} (map-indexed vector row))])
                     (str/split-lines input))))

(defn find-guard [board]
  (first (for [[i row] board
               [j c] row
               :when (= \^ c)]
           [i j])))

(defn parse-input [input]
  (let [board (parse-board input)]
    {:board (parse-board input)
     :guard {:pos (find-guard board) :dir [-1 0]}}))

(def clockwise-dirs [[-1 0] [0 1] [1 0] [0 -1]])

(def turn-right (zipmap clockwise-dirs (rest (cycle clockwise-dirs))))

(defn find-path-1 [{board :board {:keys [pos dir path]} :guard :as state}]
  (let [next-pos (mapv + pos dir)]
    (if (path {:pos next-pos :dir dir})
      ::loop
      (when-let [c (get-in board next-pos)]
        (if (#{\. \^} c)
          (-> state
              (assoc-in [:guard :pos] next-pos)
              (update-in [:guard :path] conj {:pos next-pos :dir dir}))
          (update-in state [:guard :dir] turn-right))))))
  
(defn find-path [state]
  (let [next-state (find-path-1 state)]
    (case next-state
      nil state
      ::loop ::loop
      (recur next-state))))

;; Part 1

(defn part1 [input]
  (-> (parse-input input)
      (update :guard (fn [guard] (assoc guard :path #{guard})))
      (find-path)
      (get-in [:guard :path])
      (->> (map :pos))
      (set)
      (count)))

;; Part 2

;; Slow
(defn find-obstruction-positions-resulting-in-loop-2 [state]
  (loop [obstruction-positions {}
         state state]
    (if-let [next-state (find-path-1 state)]
      (let [old-guard-pos (get-in state [:guard :pos])
            new-guard-pos (get-in next-state [:guard :pos])]
        (if (or (= old-guard-pos new-guard-pos)
                (contains? obstruction-positions new-guard-pos))
          (recur obstruction-positions next-state)
          (recur (assoc obstruction-positions new-guard-pos (= ::loop (find-path (update state :board assoc-in new-guard-pos \#))))
                 next-state)))
      (->> obstruction-positions
           (filter val)
           (keys)))))

(defn part2 [input]
  (-> (parse-input input)
      (update :guard (fn [guard] (assoc guard :path #{guard})))
      (find-obstruction-positions-resulting-in-loop-2)
      (set)
      (count)))

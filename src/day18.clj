(ns day18
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (map
   (fn [line]
     (map parse-long (str/split line #",")))
   (str/split-lines input)))

(defn neighbors [{bytes :bytes [max-x max-y] :size} pos]
  (->> [[0 1] [0 -1] [1 0] [-1 0]]
       (map #(mapv + pos %))
       (filter (fn [[x y]]
                 (and (<= 0 x max-x)
                      (<= 0 y max-y))))
       (remove bytes)))

(defn find-shortest-path [world start end]
  (loop [wip {start 0}
         visited #{start}]
    (when-let [[pos distance] (first (sort-by val wip))]
      (if (= end pos)
        distance
        (recur (merge-with min
                           (dissoc wip pos)
                           (zipmap (remove visited (neighbors world pos))
                                   (repeat (inc distance))))
               (conj visited pos))))))

;; Part 1

(defn part1 [input]
  (find-shortest-path {:bytes (set (take 1024 (parse-input input)))
                       :size  [70 70]}
                      [0 0]
                      [70 70]))

;; Part 2

(defn part2 [input]
  (let [bytes-seq (parse-input input)]
    (loop [bytes (set (take 1024 bytes-seq))
           bytes-seq (drop 1024 bytes-seq)]
      (when-let [byte (first bytes-seq)]
        (if (find-shortest-path {:bytes (conj bytes byte)
                                 :size [70 70]}
                                [0 0]
                                [70 70])
          (recur (conj bytes byte)
                 (rest bytes-seq))
          (str/join "," byte))))))

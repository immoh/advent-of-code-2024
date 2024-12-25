(ns day25
  (:require
    [clojure.string :as str]))

(defn parse-lock-or-key [s]
  (let [grid (into {} (for [[i line] (map-indexed vector (str/split-lines s))
                            [j c] (map-indexed vector line)]
                        [[i j] c]))]

    (merge {:type (if (= \# (grid [0 0])) :locks :keys)}
           {:pins (map (fn [x]
                         (dec (count (filter (fn [y] (= \# (grid [y x]))) (range 7)))))
                       (range 5))})))

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map parse-lock-or-key)
       (group-by :type)
       (map (fn [[k v]]
              [k (map :pins v)]))
       (into {})))

(defn key-fits-to-lock? [[lock key]]
  (every? (partial >= 5) (map + lock key)))

;; Part 1

(defn part1 [input]
  (let [{:keys [locks keys]} (parse-input input)]
    (->> (for [lock locks
               key keys]
           [lock key])
         (filter key-fits-to-lock?)
         (count))))

(ns day23
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

(defn parse-input [input]
  (let [pairs (map #(set (str/split % #"-")) (str/split-lines input))
        computers (reduce set/union pairs)]
    {:pairs pairs
     :computers computers
     :connections (zipmap computers
                          (map (fn [computer]
                                 (disj (->> pairs
                                            (filter #(% computer))
                                            (reduce set/union))
                                       computer))
                               computers))}))

;; Part 1

(defn find-triplets [{:keys [computers connections]}]
  (set (for [c1 computers
             c2 (connections c1)
             c3 (set/intersection (connections c1) (connections c2))]
         #{c1 c2 c3})))

(defn part1 [input]
  (->> (parse-input input)
       (find-triplets)
       (filter (partial some #(str/starts-with? % "t")))
       (count)))

;; Part 2

(defn lan-party [connections pair]
  (loop [party (set pair)]
    (if-let [new-connection (->> party
                              (map connections)
                              (reduce set/intersection)
                              (first))]
      (recur (conj party new-connection))
      party)))

(defn part2 [input]
  (let [{:keys [pairs connections]} (parse-input input)]
    (->> pairs
         (map (partial lan-party connections))
         (sort-by count)
         (last)
         (sort)
         (str/join ","))))

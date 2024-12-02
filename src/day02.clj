(ns day02
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (map parse-long (str/split line #" ")))
       (str/split-lines input)))

;; Part 1

(defn safe? [levels]
  (let [distances (map (partial apply -) (partition 2 1 levels))]
    (and (apply = (map math/signum distances))
         (every? #(<= 1 % 3) (map abs distances)))))

(defn part1 [input]
  (->> input
       (parse-input)
       (filter safe?)
       (count)))

;; Part 2

(defn safe-with-dampener? [levels]
  (some safe? (into [levels]
                    (map (fn [i]
                           (filter identity (assoc (vec levels) i nil)))
                         (range (count levels))))))

(defn part2 [input]
  (->> input
       (parse-input)
       (filter safe-with-dampener?)
       (count)))

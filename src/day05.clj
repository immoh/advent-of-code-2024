(ns day05
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (let [[rules updates] (str/split input #"\n\n")]
    {:rules   (->> (str/split-lines rules)
                   (map (fn [rule]
                          (map parse-long (str/split rule #"\|")))))
     :updates (->> (str/split-lines updates)
                   (map (fn [update]
                          (map parse-long (str/split update #",")))))}))

(defn rule-applies? [update-indices [a b]]
  (let [i (get update-indices a)
        j (get update-indices b)]
    (or (not i) (not j) (< i j))))

(defn valid-update? [rules update]
  (every? (partial rule-applies? (zipmap update (range))) rules))

(defn middle-element [coll]
  (nth coll (/ (dec (count coll)) 2)))

;; Part 1

(defn part1 [input]
  (let [{:keys [updates rules]} (parse-input input)]
    (->> updates
         (filter (partial valid-update? rules))
         (map middle-element)
         (reduce +))))

;; Part 2

(defn find-offending-index [rules update]
  (->> (range (dec (count update)))
       (remove (fn [i]
                 (valid-update? rules [(get update i) (get update (inc i))])))
       (first)))

(defn sort-1 [rules update]
  (let [update (vec update)]
    (if-let [i (find-offending-index rules update)]
      (let [x (get update i)
            y (get update (inc i))]
        (-> update
            (assoc i y)
            (assoc (inc i) x)))
      update)))

;; This is slow
(defn sort-by-rules [rules update]
  (let [sorted-update (sort-1 rules update)]
    (if (= update sorted-update)
      sorted-update
      (recur rules sorted-update))))

(defn part2 [input]
  (let [{:keys [updates rules]} (parse-input input)]
    (->> updates
         (remove (partial valid-update? rules))
         (map (partial sort-by-rules rules))
         (map middle-element)
         (reduce +))))

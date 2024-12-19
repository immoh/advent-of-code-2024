(ns day19
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (let [[patterns designs] (str/split input #"\n\n")]
    {:patterns (set (str/split patterns #", "))
     :designs  (str/split-lines designs)}))

(defn match-patterns [patterns partial-design]
  (keep (fn [pattern]
          (when (str/starts-with? partial-design pattern)
            (subs partial-design (count pattern))))
        patterns))

(declare possible-arrangements-count)

(defn possible-arrangements-count* [patterns design]
  (+ (if (patterns design) 1 0)
     (reduce + (map (partial possible-arrangements-count patterns)
                    (match-patterns patterns design)))))

(def possible-arrangements-count (memoize possible-arrangements-count*))

;; Part 1

(defn part1 [input]
  (let [{:keys [patterns designs]} (parse-input input)]
    (->> designs
         (filter (comp pos? (partial possible-arrangements-count patterns)))
         (count))))

;; Part 2

(defn part2 [input]
  (let [{:keys [patterns designs]} (parse-input input)]
    (->> designs
         (map (partial possible-arrangements-count patterns))
         (reduce +))))

(ns day07
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (let [[test-value & parameters] (map parse-long (str/split line #"\:? "))]
           {:test-value test-value
            :parameters parameters}))
       (str/split-lines input)))

(defn equation-possible? [ops {:keys [test-value parameters results] :as x}]
  (if-let [parameter (first parameters)]
    (recur ops
           {:test-value test-value
            :parameters (rest parameters)
            :results    (if (seq results)
                          (->> results
                               (mapcat (fn [result]
                                         (map (fn [op]
                                                (op result parameter))
                                              ops)))
                               (filter #(<= % test-value))
                               (set))
                          #{parameter})})
    (results test-value)))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (filter (partial equation-possible? [+ *]))
       (map :test-value)
       (reduce +)))

;; Part 2

(defn || [x y]
  (parse-long (str x y)))

(defn part2 [input]
  (->> (parse-input input)
       (filter (partial equation-possible? [+ * ||]))
       (map :test-value)
       (reduce +)))

(ns day24
  (:require
    [clojure.string :as str]))

(defn parse-gates [gates]
  (into {} (map (fn [s]
                  (let [[gate value] (str/split s #": ")]
                    [gate [:value (parse-long value)]]))
                (str/split-lines gates))))

(defn parse-connections [connections]
  (into {} (map (fn [s]
                  (let [[input output] (str/split s #" -> ")
                        [gate1 operator gate2] (str/split input #" ")]
                    [output [(keyword (str/lower-case operator)) gate1 gate2]]))
                (str/split-lines connections))))

(defn parse-input [input]
  (let [[gates connections] (str/split input #"\n\n")]
    (merge
      (parse-gates gates)
      (parse-connections connections))))

(defn gate-names-starting-with [gate-map prefix]
  (->> (keys gate-map)
       (filter #(str/starts-with? % prefix))
       (sort)
       (reverse)))

(declare eval-gate)

(defn eval-gate* [gate-map gate]
  (let [[op arg1 arg2] (get gate-map gate)]
    (case op
      :value arg1
      :and (bit-and (eval-gate gate-map arg1) (eval-gate gate-map arg2))
      :or (bit-or (eval-gate gate-map arg1) (eval-gate gate-map arg2))
      :xor (bit-xor (eval-gate gate-map arg1) (eval-gate gate-map arg2)))))

(def eval-gate (memoize eval-gate*))

(defn as-decimal [bits]
  (Long/parseLong (apply str bits) 2))

(defn decimal-value-of-prefix [gate-map prefix]
  (->> (gate-names-starting-with gate-map prefix)
       (map (partial eval-gate gate-map))
       (as-decimal)))

;; Part 1

(defn part1 [input]
  (decimal-value-of-prefix (parse-input input) "z"))

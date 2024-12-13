(ns day13
  (:require
    [clojure.string :as str]))

(defn parse-coords [line]
  (map parse-long (re-seq #"\d+" line)))

(defn parse-input [input]
  (map (fn [machine]
         (zipmap [:button-a :button-b :prize]
                 (map parse-coords (str/split-lines machine))))
       (str/split input #"\n\n")))

;; Simply solve the two-variable equation system – there is only one solution
(defn find-min-tokens-to-win [{[a-x a-y] :button-a
                               [b-x b-y] :button-b
                               [prize-x prize-y] :prize}]
  (let [b (/ (- (* a-x prize-y) (* a-y prize-x))
             (- (* a-x b-y) (* b-x a-y)))]
    (when (integer? b)
      (let [a (/ (- prize-x (* b b-x))
                 a-x)]
        (when (integer? a)
          (+ (* 3 a) b))))))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (keep find-min-tokens-to-win)
       (reduce +)))

;; Part 2

(defn adjust-prize-coords [f machine]
  (update machine :prize (partial mapv f)))

(defn part2 [input]
  (->> (parse-input input)
       (map (partial adjust-prize-coords (partial + 10000000000000)))
       (keep find-min-tokens-to-win)
       (reduce +)))

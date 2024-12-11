(ns day11
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (map parse-long (str/split input #" ")))

(defn split-into-two [stone]
  (let [stone-str (str stone)
        digit-count (count stone-str)
        new-digit-count (/ digit-count 2)]
    (map parse-long [(subs stone-str 0 new-digit-count)
                     (subs stone-str new-digit-count)])))

(defn blink [stone]
  (cond
    (zero? stone)
    [1]

    (even? (count (str stone)))
    (split-into-two stone)

    :else
    [(* 2024 stone)]))

(declare stone-count)

(defn stone-count* [remaining-iterations stone]
  (if (zero? remaining-iterations)
    1
    (reduce + (map (partial stone-count (dec remaining-iterations)) (blink stone)))))

(def stone-count (memoize stone-count*))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (map (partial stone-count 25))
       (reduce +)))

;; Part 2

(defn part2 [input]
  (->> (parse-input input)
       (map (partial stone-count 75))
       (reduce +)))

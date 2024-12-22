(ns day22
  (:require
    [clojure.math :as math]
    [clojure.string :as str]))

(defn parse-input [input]
  (map parse-long (str/split-lines input)))

(defn mix [n s]
  (bit-xor n s))

(defn prune [n]
  (mod n 16777216))

(defn floor [n]
  (long (math/floor n)))


(defn next-secret-number-1 [n]
  (-> n
      (* 64)
      (mix n)
      (prune)))

(defn next-secret-number-2 [n]
  (-> n
      (/ 32)
      (floor)
      (mix n)
      (prune)))

(defn next-secret-number-3 [n]
  (-> n
      (* 2048)
      (mix n)
      (prune)))

(defn next-secret-number [n]
  (-> n
      (next-secret-number-1)
      (next-secret-number-2)
      (next-secret-number-3)))

(defn nth-secret-number [n s]
  (nth (iterate next-secret-number s) n))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (map (partial nth-secret-number 2000))
       (reduce +)))

;; Part 2

(defn changes [prices]
  (->> prices
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defn prices [seed]
  (->> seed
       (iterate next-secret-number)
       (take 2001)
       (map #(mod % 10))))

(defn change-seq-to-price [seed]
  (let [prices (prices seed)
        changes (changes prices)]
    (map vector
         (partition 4 1 changes)
         (drop 4 prices))))

(defn sequence->first-price [change-seq-to-price-seq]
  (->> change-seq-to-price-seq
       (group-by first)
       (map (fn [[k v]]
              [k (first (map second v))]))
       (into {})))

(defn part2 [input]
  (->> (parse-input input)
       (map change-seq-to-price)
       (map sequence->first-price)
       (reduce (partial merge-with +))
       (sort-by val)
       (last)
       (second)))

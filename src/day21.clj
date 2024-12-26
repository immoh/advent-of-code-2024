(ns day21
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

(defn parse-keypad [keypad]
  (into {} (for [[i line] (map-indexed vector (str/split-lines keypad))
                 [j button] (map-indexed vector line)
                 :when (not= \space button)]
             [[i j] button])))


(def numerical-keypad (parse-keypad "789\n456\n123\n 0A"))

(def directional-keypad (parse-keypad " ^A\n<v>"))

(defn possible-key-sequences [keypad initial-key target-key]
  (let [[y1 x1] (key (first (filter #(= initial-key (val %)) keypad)))
        [y2 x2] (key (first (filter #(= target-key (val %)) keypad)))]
    (distinct (concat (when (keypad [y2 x1])
                        [(apply str (concat (repeat (abs (- y2 y1)) (if (< y1 y2) \v \^))
                                            (repeat (abs (- x2 x1)) (if (< x1 x2) \> \<))
                                            [\A]))])
                      (when (keypad [y1 x2])
                        [(apply str (concat (repeat (abs (- x2 x1)) (if (< x1 x2) \> \<))
                                            (repeat (abs (- y2 y1)) (if (< y1 y2) \v \^))
                                            [\A]))])))))

(declare min-presses-1)

(defn min-presses-1* [level directional-keyboard-count keypad initial-key target-key]
  (if (= directional-keyboard-count level)
    1
    (apply min (map (fn [key-sequence]
                      (reduce + (map (fn [[key1 key2]]
                                       (min-presses-1 (inc level) directional-keyboard-count directional-keypad key1 key2))
                                     (partition 2 1 (str "A" key-sequence)))))
                    (possible-key-sequences keypad initial-key target-key)))))

(def min-presses-1 (memoize min-presses-1*))

(defn min-presses [directional-keyboard-count key-sequence]
  (reduce + (map (fn [[key1 key2]]
                   (min-presses-1 0 directional-keyboard-count numerical-keypad key1 key2))
                 (partition 2 1 (str "A" key-sequence)))))

(defn complexity [directional-keyboard-count code]
  (* (parse-long (subs code 0 3))
     (min-presses directional-keyboard-count code)))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (map (partial complexity 3))
       (reduce +)))

;; Part 2

(defn part2 [input]
  (->> (parse-input input)
       (map (partial complexity 26))
       (reduce +)))

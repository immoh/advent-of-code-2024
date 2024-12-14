(ns day14
  (:require
    [clojure.string :as str]
    [clojure.math :as math]))

(defn parse-input [input]
  (map (fn [line]
         (let [[x y dx dy] (map parse-long (re-seq #"-?\d+" line))]
           {:position [x y]
            :velocity [dx dy]}))
       (str/split-lines input)))

(defn move-robot [width height {[x y] :position [dx dy] :velocity :as robot}]
  (assoc robot :position [(mod (+ x dx) width) (mod (+ y dy) height)]))

(defn tick [{:keys [width height] :as world}]
  (update world :robots #(map (partial move-robot width height) %)))

(defn simulate-seconds [world seconds]
  (->> world
       (iterate tick)
       (drop seconds)
       (first)))

(defn safety-factor [{:keys [robots width height]}]
  (let [middle-x (/ (dec width) 2)
        middle-y (/ (dec height) 2)]
    (->> robots
         (map :position)
         (map (fn [[x y]] [(math/signum (- x middle-x))
                           (math/signum (- y middle-y))]))
         (filter #(not-any? zero? %))
         (frequencies)
         (vals)
         (reduce *))))

;; Part 1

(defn part1 [input]
  (-> {:robots (parse-input input)
       :width 101
       :height 103}
      (simulate-seconds 100)
      (safety-factor)))

;; Part 2


(defn print-world [{:keys [robots width height]}]
  (let [robot-positions (frequencies (map :position robots))]
    (doseq [y (range height)]
      (println (apply str (map (fn [x]
                                 (get robot-positions [x y] \.))
                               (range width)))))))

(defn part2 [input]
  (loop [world (-> {:robots (parse-input input)
                    :width  101
                    :height 103})
         i 0]
    ;; Assumption: all robots are in distinct position when
    (if (= (count (set (map :position (:robots world))))
           (count (map :position (:robots world))))
      (do
        (print-world world)
        (println i)
        (Thread/sleep 5000)))
    (recur (tick world) (inc i))))

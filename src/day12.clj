(ns day12
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (into {} (for [[i line] (map-indexed vector (str/split-lines input))
                 [j c] (map-indexed vector line)]
             [[i j] c])))

(defn neighbors [plot]
  (map (partial mapv + plot) [[-1 0] [1 0] [0 -1] [0 1]]))

(defn expand-plot [plot]
  (into [plot] (neighbors plot)))

(defn extract-region
  ([garden]
   (extract-region garden #{(first (keys garden))}))
  ([garden region]
   (let [label (get garden (first region))
         new-region (->> region
                         (mapcat expand-plot)
                         (set)
                         (filter #(= label (get garden %)))
                         (set))]
     (if (= region new-region)
       region
       (recur garden new-region)))))

(defn regions
  ([garden]
   (regions garden #{}))
  ([garden regions]
   (if (seq garden)
     (let [region (extract-region garden)]
       (recur (reduce dissoc garden region) (conj regions region)))
     regions)))

(defn area [region]
  (count region))

(defn plot-fence-count [garden plot]
  (let [label (get garden plot)]
    (->> (neighbors plot)
         (remove #(= label (get garden %)))
         (count))))

(defn perimeter [garden region]
  (reduce + (map (partial plot-fence-count garden) region)))


(defn cost [garden region]
  (* (area region) (perimeter garden region)))

;; Part 1

(defn part1 [input]
  (let [garden (parse-input input)]
    (->> garden
         (regions)
         (map (partial cost garden))
         (reduce +))))

;; Part 2

(defn- count-continuous [dir plots]
  (->> plots
       (partition 2 1)
       (remove (fn [[p1 p2]] (= dir (mapv - p2 p1))))
       (count)
       (inc)))

(defn sides-in-dir [garden region dir]
  (let [label (get garden (first region))
        sort-fn (if (zero? (first dir)) (comp vec reverse) identity)
        sort-dir (if (zero? (first dir)) [1 0] [0 1])]
    (->> (sort-by sort-fn region)
         (remove #(= label (get garden (mapv + % dir))))
         (count-continuous sort-dir))))

(defn sides [garden region]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map (partial sides-in-dir garden region))
       (reduce +)))

(defn cost2 [garden region]
  (* (area region) (sides garden region)))

(defn part2 [input]
  (let [garden (parse-input input)]
    (->> garden
         (regions)
         (map (partial cost2 garden))
         (reduce +))))

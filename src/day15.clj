(ns day15
  (:require
    [clojure.string :as str]))

(defn parse-warehouse [warehouse]
  (let [positions (for [[i line] (map-indexed vector (str/split-lines warehouse))
                        [j c] (map-indexed vector line)
                        :let [entity ({\# :walls
                                       \O :boxes
                                       \@ :robot} c)]
                        :when entity]
                    {:pos [i j] :entity entity})]
    (-> positions
        (->> (group-by :entity))
        (update :robot (comp :pos first))
        (update :walls #(set (map :pos %)))
        (update :boxes #(set (map (comp hash-set :pos) %))))))

(defn parse-moves [moves]
  (keep {\^ [-1 0]
         \> [0 1]
         \v [1 0]
         \< [0 -1]}
        moves))

(defn parse-input [input]
  (let [[warehouse moves] (str/split input #"\n\n")]
    {:warehouse (parse-warehouse warehouse)
     :moves     (parse-moves moves)}))

;; For debugging
(defn print-warehouse [{:keys [robot boxes walls]}]
  (doseq [y (range (reduce min (map first walls)) (inc (reduce max (map first walls))))]
    (println (->> (range (reduce min (map second walls)) (inc (reduce max (map second walls))))
                  (map (fn [x]
                         (let [box (first (filter #(% [y x]) boxes))]
                           (cond
                             (and box (= 1 (count box))) \O
                             (and box (= [y x] (first (sort box)))) \[
                             box \]
                             (= robot [y x]) \@
                             (walls [y x]) \#
                             :else \.))))
                  (reduce str)))))


(defn move [pos dir]
  (mapv + pos dir))

(defn find-moving-boxes [boxes new-pos dir]
  (loop [boxes-to-move (set (filter #(% new-pos) boxes))]
    (let [new-boxes-to-move (set (concat boxes-to-move (filter #(some % (->> boxes-to-move
                                                                             (mapcat identity)
                                                                             (map (fn [pos] (move pos dir)))))
                                                               boxes)))]
      (if (= new-boxes-to-move boxes-to-move)
        boxes-to-move
        (recur new-boxes-to-move)))))

(defn apply-move [{:keys [robot walls boxes] :as warehouse} dir]
  (let [new-pos (move robot dir)]
    (if (walls new-pos)
      warehouse
      (let [boxes-to-move (find-moving-boxes boxes new-pos dir)
            moved-boxes (map (fn [box]
                               (set (map #(move % dir) box)))
                             boxes-to-move)]
        (if (some walls (mapcat identity moved-boxes))
          warehouse
          (-> warehouse
              (assoc :robot new-pos)
              (update :boxes (fn [boxes]
                               (-> (reduce disj boxes boxes-to-move)
                                   (into moved-boxes))))))))))

(defn gps-coordinate [positions]
  (let [[y x] (first (sort positions))]
    (+ (* 100 y) x)))

;; Part 1

(defn part1 [input]
  (let [{:keys [moves warehouse]} (parse-input input)]
    (->> moves
         (reduce apply-move warehouse)
         :boxes
         (map gps-coordinate)
         (reduce +))))


;; Part 2

(defn pos-in-widened-wh [[y x]]
  [y (* 2 x)])

(defn widen-warehouse [warehouse]
  (-> warehouse
      (update :robot pos-in-widened-wh)
      (update :walls (fn [walls]
                       (let [walls (map pos-in-widened-wh walls)]
                         (set (concat walls (map #(move % [0 1]) walls))))))
      (update :boxes (fn [boxes]
                       (set (map (fn [box]
                                   (let [pos (pos-in-widened-wh (first box))]
                                     #{pos (move pos [0 1])}))
                                 boxes))))))

(defn part2 [input]
  (let [{:keys [moves warehouse]} (parse-input input)]
    (->> moves
         (reduce apply-move (widen-warehouse warehouse))
         :boxes
         (map gps-coordinate)
         (reduce +))))

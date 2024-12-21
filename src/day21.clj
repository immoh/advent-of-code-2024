(ns day21
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

(defn parse-keypad [keypad]
  (let [keypad (into {} (for [[i line] (map-indexed vector (str/split-lines keypad))
                              [j button] (map-indexed vector line)
                              :when (not= \space button)]
                          [[i j] button]))]
    {:keypad keypad
     :pos (key (first (filter #(= \A (val %)) keypad)))}))


(def numerical-keypad (parse-keypad "789\n456\n123\n 0A"))

(def directional-keypad (parse-keypad " ^A\n<v>"))

(defn apply-press-1 [machine action]
  (if (string? machine)
    {:machine (str machine action)}
    (if (= \A action)
      {:machine machine :out (get-in machine [:keypad (:pos machine)])}
      (let [new-pos (mapv + (:pos machine) ({\< [0 -1] \> [0 1] \^ [-1 0] \v [1 0]} action))]
        (when (get-in machine [:keypad new-pos])
          {:machine (assoc machine :pos new-pos)})))))

(defn apply-press [state action]
  (loop [wip state
         input action
         result-state []]
    (if-let [machine (first wip)]
      (if input
        (when-let [{new-machine :machine out :out} (apply-press-1 machine input)]
          (recur (rest wip) out (conj result-state new-machine)))
        (recur (rest wip) nil (conj result-state machine)))
      result-state)))

(defn neighbors [state]
  (keep (partial apply-press state) "<>^vA"))

(defn find-min-button-presses [keypads code]
  (let [initial-state (conj (vec keypads) "")]
    (loop [wip {initial-state 0}
           visited #{}]
      (when-let [[current distance] (first (sort-by val wip))]
        (cond
          (= code (last current))
          distance

          (not (str/starts-with? code (last current)))
          (recur (dissoc wip current) (conj visited current))

          :else
          (recur (merge-with min
                             (dissoc wip current)
                             (zipmap (neighbors current)
                                     (repeat (inc distance))))
                 (conj visited current)))))))

(defn complexity [keypads code]
  (* (parse-long (subs code 0 3))
     (find-min-button-presses keypads code)))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (map (partial complexity [directional-keypad directional-keypad numerical-keypad]))
       (reduce +)))

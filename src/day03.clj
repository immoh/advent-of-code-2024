(ns day03)

(defn parse-input
  ([input]
   (parse-input input []))
  ([input instructions]
   (if (seq input)
     (if-let [[instruction & groups] (or (re-find #"^(do)\(\)" input)
                                         (re-find #"^(don't)\(\)" input)
                                         (re-find #"^(mul)\((\d+),(\d+)\)" input))]
       (recur (subs input (count instruction))
              (conj instructions (into [(keyword (first groups))] (map parse-long (rest groups)))))
       (recur (subs input 1)
              instructions))
     instructions)))

(defn apply-instruction [{:keys [enabled?] :as state} [cmd & args]]
  (case cmd
    :mul (if enabled?
           (update state :sum + (apply * args))
           state)
    :do (assoc state :enabled? true)
    :don't (assoc state :enabled? false)))

(defn run-instructions [instructions]
  (reduce apply-instruction
          {:sum 0 :enabled? true}
          instructions))

;; Part 1

(defn part1 [input]
  (->> input
       (parse-input)
       (filter (comp #{:mul} first))
       (run-instructions)
       :sum))

;; Part 2

(defn part2 [input]
  (->> input
       (parse-input)
       (run-instructions)
       :sum))

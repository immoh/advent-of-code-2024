(ns day17
  (:require
    [clojure.math :as math]
    [clojure.string :as str]))

(defn parse-value [s]
  (parse-long (second (str/split s #": "))))

(defn parse-values [s]
  (-> (str/split s #": ")
      (second)
      (str/split #",")
      (->> (mapv parse-long))))

(defn parse-input [input]
  (let [[a b c _ program] (str/split-lines input)]
    {:registers {:a (parse-value a)
                 :b (parse-value b)
                 :c (parse-value c)}
     :program   (parse-values program)}))


(defn combo-operand [{{:keys [a b c]} :registers} operand]
  (get [0 1 2 3 a b c] operand))

(defmulti apply-op (fn [_ [op _]]
                     (get [:adv :bxl :bst :jnz :bxc :out :bdv :cdv] op)))

(defmethod apply-op :adv [{{:keys [a]} :registers :as state} [_ operand]]
  {:registers {:a (math/floor-div a (long (math/pow 2 (combo-operand state operand))))}})

(defmethod apply-op :bxl [{{:keys [b]} :registers} [_ operand]]
  {:registers {:b (bit-xor b operand)}})

(defmethod apply-op :bst [state [_ operand]]
  {:registers {:b (mod (combo-operand state operand) 8)}})

(defmethod apply-op :jnz [{{:keys [a]} :registers} [_ operand]]
  (when-not (zero? a)
    {:instruction operand}))

(defmethod apply-op :bxc [{{:keys [b c]} :registers} [_ _]]
  {:registers {:b (bit-xor b c)}})

(defmethod apply-op :out [state [_ operand]]
  {:out (mod (combo-operand state operand) 8)})

(defmethod apply-op :bdv [{{:keys [a]} :registers :as state} [_ operand]]
  {:registers {:b (math/floor-div a (long (math/pow 2 (combo-operand state operand))))}})

(defmethod apply-op :cdv [{{:keys [a]} :registers :as state} [_ operand]]
  {:registers {:c (math/floor-div a (long (math/pow 2 (combo-operand state operand))))}})

(defn run-program [{:keys [registers program]}]
  (loop [{:keys [instruction] :as state} {:registers registers
                                          :instruction 0}
         program-out []]
    (if (< instruction (count program))
      (let [{:keys [registers out instruction]} (apply-op state (->> program (drop instruction) (take 2)))]
        (recur (-> state
                   (update :registers merge registers)
                   (update :instruction (if instruction (constantly instruction) (partial + 2))))
               (if out
                 (conj program-out out)
                 program-out)))
      program-out)))

;; Part 1

(defn part1 [input]
  (-> (parse-input input)
      (run-program)
      (->> (str/join ","))))

;; part2

(defn run-program-with-a [state a]
  (-> state
      (assoc-in [:registers :a] a)
      (run-program)))

(defn base8-to-decimal [coefficients length]
  (reduce + (map (fn [cofx power]
                   (* cofx (long (math/pow 8 power))))
                 coefficients
                 (reverse (range length)))))

;; nth bit (staring from 0) in program output changes when a changes by 8^n
; a = x0 * 8^0 + ... + xn * 8^n
; we need to find smallest coefficients x [0..7] that result in same bit values
(defn part2 [input]
  (let [{:keys [program] :as initial-state} (parse-input input)]
    (loop [wip [[]]]
      (when-let [coefficients (first wip)]
        (if (= (count program) (count coefficients))
          (base8-to-decimal coefficients (count program))
          (let [i (- (count program) (count coefficients) 1)]
            (recur (concat (->> (range 8)
                                (map (partial conj coefficients))
                                (filter #(= (nth program i)
                                            (nth (run-program-with-a initial-state
                                                                     (base8-to-decimal % (count program)))
                                                 i
                                                 nil))))
                           (rest wip)))))))))

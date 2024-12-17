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

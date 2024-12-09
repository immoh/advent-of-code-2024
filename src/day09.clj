(ns day09)

(defn parse-input [input]
  (->> (reduce
        (fn [{:keys [type index file-id] :as acc} n]
          (-> acc
              (assoc-in [:disk-map index] (merge {:type type
                                                  :length n}
                                                 (when (= :file type)
                                                   {:id file-id})))
              (update :type {:space :file :file :space})
              (update :index + n)
              (update :file-id (if (= :file type) inc identity))))
        {:index    0
         :type     :file
         :file-id  0
         :disk-map {}}
        (map (comp parse-long str) input))
       :disk-map))

(defn map-entries-of-type [disk-map type]
  (->> disk-map
       (sort)
       (filter #(= type (:type (val %))))))

(defn swap [disk-map first-space-index last-file-index]
  (let [{space-length :length :as space} (get disk-map first-space-index)
        {file-length :length :as file} (get disk-map last-file-index)]
    (cond
      (< space-length file-length)
      (-> disk-map
          (assoc first-space-index (assoc file :length space-length))
          (update-in [last-file-index :length] - space-length))

      (> space-length file-length)
      (-> disk-map
          (assoc first-space-index file)
          (assoc (+ first-space-index file-length) (update space :length - file-length))
          (dissoc last-file-index))

      :else
      (-> disk-map
          (assoc first-space-index file)
          (assoc last-file-index space)))))


(defn file-checksum [[start {:keys [id length]}]]
  (* (/ length 2)
     (+ (* 2 start) length -1)
     id))

(defn checksum [disk-map]
  (->> disk-map
       (filter #(= :file (:type (val %))))
       (map file-checksum)
       (reduce +)))

;; Part 1

(defn compact [disk-map]
  (let [first-space-index (-> (map-entries-of-type disk-map :space) first key)
        last-file-index (-> (map-entries-of-type disk-map :file) last key)]
    (if (< first-space-index last-file-index)
      (recur (swap disk-map first-space-index last-file-index))
      disk-map)))

(defn part1 [input]
  (->> input
       (parse-input)
       (compact)
       (checksum)))

;; Part 2

(defn compact2 [disk-map]
  (let [last-file-id (-> (map-entries-of-type disk-map :file) last val :id)]
    (reduce (fn [disk-map file-id]
              (let [[file-index file] (->> (map-entries-of-type disk-map :file)
                                           (filter (comp #{file-id} :id val))
                                           (first))
                    [space-index space] (->> (map-entries-of-type disk-map :space)
                                             (filter #(<= (:length file) (:length (val %))))
                                             (first))]
                (if (and space (< space-index file-index))
                  (swap disk-map space-index file-index)
                  disk-map)))
            disk-map
            (range last-file-id 0 -1))))

(defn part2 [input]
  (->> input
       (parse-input)
       (compact2)
       (checksum)))

(ns lab3.part1)

(defn chunk-blocks
  "Ручное разбиение на блоки через take/drop"
  [coll block-size]
  (when (<= block-size 0)
    (throw (ex-info "block-size must be positive" {:block-size block-size})))
  (loop [xs (seq coll), acc []]
    (if (seq xs)
      (let [blk (take block-size xs)]
        (if (seq blk)
          (recur (drop block-size xs) (conj acc (vec blk)))
          acc))
      acc)))

(defn pfilter
  "Параллельный (не ленивый) filter блоками через future"
  [pred coll block-size]
  (let [blocks  (chunk-blocks coll block-size)
        futures (mapv (fn [blk]
                        (future
                          (persistent!
                           (reduce (fn [acc x]
                                     (if (pred x) (conj! acc x) acc))
                                   (transient []) blk))))
                      blocks)]
    (persistent!
     (reduce (fn [acc f] (reduce conj! acc @f))
             (transient []) futures))))

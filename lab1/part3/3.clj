(ns lab1.part3)

(defn reverse-list
  [coll]
  (reduce (fn [acc x] (cons x acc)) '() coll))

(defn my-map
  [f coll]
  (let [acc (reduce (fn [acc x] (cons (f x) acc)) '() coll)]
    (reverse-list acc)))

(defn my-filter
  [pred coll]
  (let [acc (reduce (fn [acc x] (if (pred x) (cons x acc) acc)) '() coll)]
    (reverse-list acc)))

(defn distinct-preserving-order
  [xs]
  (let [seen (volatile! #{})]
    (my-filter (fn [x]
                 (if (contains? @seen x)
                   false
                   (do (vswap! seen conj x) true)))
               xs)))

(defn strings-no-adjacent-hofs
  [chars n]
  (let [chs (distinct-preserving-order (seq chars))]
    (cond
      (neg? n) '()
      (zero? n) (list "")
      (empty? chs) '()
      :else
      (let [step (fn [level _]
                   (let [lists-of-extensions
                         (my-map (fn [s]
                                   (let [lastc (when (seq s) (last s))
                                         allowed (my-filter (fn [c] (or (nil? lastc) (not= c lastc)))
                                                            chs)]
                                     (my-map (fn [c] (str s c)) allowed)))
                                 level)]
                     (reduce concat '() lists-of-extensions)))]
        (reduce step (list "") (range n))))))

(defn -main
  [& args]
  (let [[chars n-str] args]
    (if (or (nil? chars) (nil? n-str))
      (do (binding [*out* *err*]
            (println "Usage: clj -M:run <chars> <n>   e.g.  clj -M:run abc 2"))
          (System/exit 1))
      (let [n  (Long/parseLong n-str)
            xs (strings-no-adjacent-hofs chars n)]
        (loop [s xs, i 0]
          (if (seq s)
            (do (println (first s))
                (recur (rest s) (inc i)))
            (println "Total:" i)))
        (shutdown-agents)))))

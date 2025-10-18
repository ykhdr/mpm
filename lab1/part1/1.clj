(ns lab1.part1)

(defn distinct-preserving-order
  [xs]
  (letfn [(step [ys seen acc]
            (if (empty? ys)
              (seq (reverse acc))
              (let [x (first ys)]
                (if (contains? seen x)
                  (recur (rest ys) seen acc)
                  (recur (rest ys) (conj seen x) (cons x acc))))))]
    (step (seq xs) #{} '())))

(defn strings-no-adjacent-rec
  [chars n]
  (let [chs (distinct-preserving-order (seq chars))]
    (cond
      (neg? n) '()
      (zero? n) (list "")
      (empty? chs) '()
      :else
      (letfn [(extend-with
                [k prefix prev]
                (if (zero? k)
                  (list prefix)
                  (letfn [(walk-chs
                            [acc ys]
                            (if (empty? ys)
                              acc
                              (let [c (first ys)]
                                (if (= c prev)
                                  (recur acc (rest ys))
                                  (let [more (extend-with (dec k) (str prefix c) c)]
                                    (recur (concat acc more) (rest ys)))))))]
                    (walk-chs '() chs))))]
        (extend-with n "" nil)))))

(defn -main
  [& args]
  (let [[chars n-str] args]
    (if (or (nil? chars) (nil? n-str))
      (do (binding [*out* *err*]
            (println "Usage: clj -M:run <chars> <n>   e.g.  clj -M:run abc 2"))
          (System/exit 1))
      (let [n (Long/parseLong n-str)
            xs (strings-no-adjacent-rec chars n)]
        (loop [s xs, i 0]
          (if (seq s)
            (do (println (first s))
                (recur (rest s) (inc i)))
            (println "Total:" i)))
        (shutdown-agents)))))

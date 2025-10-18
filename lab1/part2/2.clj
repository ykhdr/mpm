(ns lab1.part2)

(defn distinct-preserving-order
  [xs]
  (let [s (seq xs)]
    (loop [ys s
           seen #{}
           acc  []]
      (if (seq ys)
        (let [x (first ys)]
          (if (contains? seen x)
            (recur (rest ys) seen acc)
            (recur (rest ys) (conj seen x) (conj acc x))))
        (seq acc)))))

(defn strings-no-adjacent-tail
  [chars n]
  (let [chs (distinct-preserving-order (seq chars))]
    (cond
      (neg? n) '()
      (zero? n) (list "")
      (empty? chs) '()
      :else
      (let [cv (vec chs)]
        (loop [k   0
               acc [""]]
          (if (= k n)
            (seq acc)
            (let [cnt (count acc)
                  m   (count cv)
                  out (loop [i   0
                             out []]
                        (if (= i cnt)
                          out
                          (let [s    (nth acc i)
                                slen (count s)
                                lastc (when (pos? slen)
                                        (nth s (dec slen)))
                                out2 (loop [j    0
                                            out2 out]
                                       (if (= j m)
                                         out2
                                         (let [c (nth cv j)]
                                           (if (= c lastc)
                                             (recur (inc j) out2)
                                             (recur (inc j) (conj out2 (str s c)))))))]
                            (recur (inc i) out2))))]
              (recur (inc k) out))))))))

(defn -main
  [& args]
  (let [[chars n-str] args]
    (if (or (nil? chars) (nil? n-str))
      (do (binding [*out* *err*]
            (println "Usage: clj -M:run <chars> <n>   e.g.  clj -M:run abc 2"))
          (System/exit 1))
      (let [n  (Long/parseLong n-str)
            xs (strings-no-adjacent-tail chars n)]
        (loop [s xs, i 0]
          (if (seq s)
            (do (println (first s))
                (recur (rest s) (inc i)))
            (println "Total:" i)))
        (shutdown-agents)))))

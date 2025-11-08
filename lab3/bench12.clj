(ns lab3.bench)

(load-file "part1/1.clj")
(load-file "part2/2.clj")
(require 'lab3.part1)
(require 'lab3.part2)
(alias 'P1 'lab3.part1)
(alias 'P2 'lab3.part2)

(defn now-ns [] (System/nanoTime))
(defn ms [ns] (/ ns 1.0e6))

(defn busy-pred
  "Возвращает тяжёлый CPU-предикат по параметру work."
  [work]
  (let [w (max 1 (long work))]
    (fn [^long x]
      (let [x' (double x)]
        (loop [i (long 0) s (double 0.0)]
          (if (< i w)
            (recur (unchecked-inc i)
                   (+ s (Math/sin (+ x' i))
                      (Math/cos (+ x' (* 2.0 i)))))
            (> s 0.0)))))))

(defn run-bench
  "cfg: {:n :block :inflight :work :rounds}
   Сравнивает: core/filter, P1/pfilter (неленивый), P2/pfilter (ленивый)."
  [{:keys [n block inflight work rounds]}]
  (let [xs   (vec (range n))
        pred (busy-pred work)
        bench (fn [f]
                (let [t0 (now-ns)]
                  (dotimes [_ rounds] (f))
                  (- (double (now-ns)) t0)))
        t-filter (bench #(doall (filter pred xs)))
        t-p1     (bench #(P1/pfilter pred xs block))
        t-p2     (bench #(doall (P2/pfilter pred xs block inflight)))]
    {:params  {:n n :block block :inflight inflight :work work :rounds rounds}
     :timing  {:filter_ms (ms t-filter)
               :p1_ms     (ms t-p1)
               :p2_ms     (ms t-p2)}
     :speedup {:p1_vs_filter (/ t-filter t-p1)
               :p2_vs_filter (/ t-filter t-p2)
               :p2_vs_p1     (/ t-p1 t-p2)}}))

(defn -main [& _]
  (let [cfg {:n 20000
             :block 500
             :inflight (.availableProcessors (Runtime/getRuntime))
             :work 40000
             :rounds 1}
        {:keys [params timing speedup]} (run-bench cfg)]
    (println "Benchmark params:" (:params {:params params}))
    (println (format "Timing (ms):\n  filter = %.3f\n  p1     = %.3f\n  p2     = %.3f"
                     (:filter_ms timing) (:p1_ms timing) (:p2_ms timing)))
    (println (format "Speedups:\n  p1 vs filter → %.2fx\n  p2 vs filter → %.2fx\n  p2 vs p1     → %.2fx"
                     (:p1_vs_filter speedup)
                     (:p2_vs_filter speedup)
                     (:p2_vs_p1 speedup)))))

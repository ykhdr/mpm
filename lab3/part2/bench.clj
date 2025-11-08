(ns lab3.part2.bench
  (:require [lab3.part2 :as P]))

(defn now-ns [] (System/nanoTime))
(defn ms [ns] (/ ns 1.0e6))

(defn busy-pred
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

(defn run-bench [{:keys [n block inflight work rounds]}]
  (let [xs   (vec (range n))
        pred (busy-pred work)]
    (letfn [(bench [f]
              (let [t0 (now-ns)]
                (dotimes [_ rounds] (f))
                (- (double (now-ns)) t0)))]
      (let [t1 (bench #(doall (filter pred xs)))
            t2 (bench #(doall (P/pfilter pred xs block inflight)))]
        {:n n, :block block, :inflight inflight, :work work, :rounds rounds
         :t_filter_ms (ms t1)
         :t_pfilter_ms (ms t2)
         :speedup (/ t1 t2)}))))

(defn -main [& _]
  (let [cfg {:n 20000
             :block 500
             :inflight 2; (.availableProcessors (Runtime/getRuntime))
             :work 10000
             :rounds 1}
        {:keys [t_filter_ms t_pfilter_ms speedup]} (run-bench cfg)]
    (println "Benchmark params:" cfg)
    (println (format "filter:  %.3f ms" t_filter_ms))
    (println (format "pfilter: %.3f ms" t_pfilter_ms))
    (println (format "speedup: %.2fx" (double speedup)))))

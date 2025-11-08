(ns lab2.bench
  (:require [clojure.string :as str]))

(load-file "part1/1.clj")
(load-file "part2/2.clj")

(defn now-ns [] (System/nanoTime))
(defn ms [ns] (/ ns 1.0e6))
(defn fmt-ms [x] (format "%.3f" x))

(defn trap-integral
  [f h x]
  (let [h (double h)
        sign (if (neg? x) -1.0 1.0)
        X (Math/abs (double x))
        n (long (Math/floor (/ X h)))
        r (- X (* n h))]
    (loop [k 0, acc 0.0, prev (f 0.0)]
      (if (< k n)
        (let [t1 (* (inc k) h)
              v1 (f t1)
              area (* 0.5 h (+ prev v1))]
          (recur (inc k) (+ acc area) v1))
        (let [vn (if (zero? n) (f 0.0) prev)
              vx (f X)
              last-area (* 0.5 r (+ vn vx))]
          (* sign (+ acc last-area)))))))

(defn gen-fixed-grid
  [xmin xmax total-npts]
  (let [step (/ (- xmax xmin) (double (dec total-npts)))]
    (vec (map #(+ xmin (* step %)) (range total-npts)))))

(defn run-bench-append-3
  [{:keys [f h xmin xmax base-npts append-npts rounds]}]
  (let [total-npts (+ base-npts (* (max 0 (dec rounds)) append-npts))
        xs-full    (gen-fixed-grid xmin xmax total-npts)
        xs-rounds  (mapv (fn [i]
                           (subvec xs-full 0 (+ base-npts (* i append-npts))))
                         (range rounds))
        F-naive (fn [x] (trap-integral f h x))
        F-memo  ((resolve 'lab2.part1/make-integral-op-memo) f h)
        F-stream ((resolve 'lab2.part2/make-integral-op-stream) f h)

        t-naive (mapv (fn [xs]
                        (let [t0 (now-ns)]
                          (doseq [x xs] (F-naive x))
                          (- (double (now-ns)) t0)))
                      xs-rounds)
        t-memo  (mapv (fn [xs]
                        (let [t0 (now-ns)]
                          (doseq [x xs] (F-memo x))
                          (- (double (now-ns)) t0)))
                      xs-rounds)
        t-stream (mapv (fn [xs]
                         (let [t0 (now-ns)]
                           (doseq [x xs] (F-stream x))
                           (- (double (now-ns)) t0)))
                       xs-rounds)

        sum-naive  (reduce + t-naive)
        sum-memo   (reduce + t-memo)
        sum-stream (reduce + t-stream)]

    {:params {:h h :xmin xmin :xmax xmax
              :base-npts base-npts :append-npts append-npts :rounds rounds}
     :per-round
     (mapv (fn [i nt mt st]
             {:round (inc i)
              :points (+ base-npts (* i append-npts))
              :added  (if (zero? i) 0 append-npts)
              :naive_ms  (ms nt)
              :memo_ms   (ms mt)
              :stream_ms (ms st)})
           (range rounds) t-naive t-memo t-stream)
     :total {:naive_ms  (ms sum-naive)
             :memo_ms   (ms sum-memo)
             :stream_ms (ms sum-stream)
             :speedup   {:memo_vs_naive   (double (/ sum-naive sum-memo))
                         :stream_vs_naive (double (/ sum-naive sum-stream))
                         :memo_vs_stream  (double (/ sum-stream sum-memo))}}}))

(defn print-report [{:keys [params per-round total]}]
  (println (str/join
            "\n"
            [(format "Incremental benchmark:")
             (format "  range      = [%.1f, %.1f]" (-> params :xmin) (-> params :xmax))
             (format "  h          = %g"          (-> params :h))
             (format "  base-npts  = %d"          (-> params :base-npts))
             (format "  append     = +%d per round" (-> params :append-npts))
             (format "  rounds     = %d"          (-> params :rounds))
             ""
             "Per-round timing (ms):"]))
  (doseq [{:keys [round points added naive_ms memo_ms stream_ms]} per-round]
    (println (format "  round %-2d  points=%-6d (+%d)   naive=%8s   memo=%8s   stream=%8s"
                     round points added (fmt-ms naive_ms) (fmt-ms memo_ms) (fmt-ms stream_ms))))
  (println
   (str "\nTotal:\n"
        (format "  naive   = %s ms\n"  (fmt-ms (-> total :naive_ms)))
        (format "  memo    = %s ms\n"  (fmt-ms (-> total :memo_ms)))
        (format "  stream  = %s ms\n"  (fmt-ms (-> total :stream_ms)))
        (format "Speedup:\n")
        (format "  memo vs naive    → %.2f× faster\n"   (-> total :speedup :memo_vs_naive))
        (format "  stream vs naive  → %.2f× faster\n"   (-> total :speedup :stream_vs_naive))
        (format "  memo vs stream   → %.2f× faster\n"   (-> total :speedup :memo_vs_stream)))))

(defn -main [& _]
  (let [cfg {:f #(Math/sin (double %))
             :h 1.0e-3
             :xmin 0.0 :xmax 50.0
             :base-npts 100
             :append-npts 40
             :rounds 10}
        res (run-bench-append-3 cfg)]
    (print-report res)))

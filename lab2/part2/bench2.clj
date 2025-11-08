(ns lab2.part2.bench
  (:require [clojure.string :as str]))

(load-file "2.clj")

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

(defn gen-increasing [xmin xmax n]
  (let [step (/ (- xmax xmin) (double (dec n)))]
    (vec (map #(+ xmin (* step %)) (range n)))))

(defn run-bench-append
  [{:keys [f h xmin xmax base-npts append-npts rounds]}]
  (let [total-npts (+ base-npts (* (max 0 (dec rounds)) append-npts))
        xs-full    (gen-increasing xmin xmax total-npts)
        xs-rounds  (mapv (fn [i]
                           (let [cnt (+ base-npts (* i append-npts))]
                             (subvec xs-full 0 cnt)))
                         (range rounds))

        F0   (fn [x] (trap-integral f h x))
        Fs   (lab2.part2/make-integral-op-stream f h)]

    (let [naive-times
          (mapv (fn [xs]
                  (let [t0 (now-ns)]
                    (doseq [x xs] (F0 x))
                    (- (double (now-ns)) t0)))
                xs-rounds)

          stream-times
          (mapv (fn [xs]
                  (let [t0 (now-ns)]
                    (doseq [x xs] (Fs x))
                    (- (double (now-ns)) t0)))
                xs-rounds)

          sum-naive  (reduce + naive-times)
          sum-stream (reduce + stream-times)
          speedup    (/ sum-naive sum-stream)]

      {:params {:h h :xmin xmin :xmax xmax
                :base-npts base-npts :append-npts append-npts
                :rounds rounds}
       :per-round
       (mapv (fn [i nt st]
               {:round (inc i)
                :points (+ base-npts (* i append-npts))
                :added  (if (zero? i) 0 append-npts)
                :naive_ms  (ms nt)
                :stream_ms (ms st)})
             (range rounds) naive-times stream-times)
       :total {:naive_ms  (ms sum-naive)
               :stream_ms (ms sum-stream)
               :speedup   speedup}})))

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
  (doseq [{:keys [round points added naive_ms stream_ms]} per-round]
    (println (format "  round %-2d  points=%-6d (+%d)   naive=%8s   stream=%8s"
                     round points added (fmt-ms naive_ms) (fmt-ms stream_ms))))
  (println (str/join
            "\n"
            [""
             (format "Total:")
             (format "  naive   = %s ms"  (fmt-ms (:naive_ms total)))
             (format "  stream  = %s ms"  (fmt-ms (:stream_ms total)))
             (format "  speedup = %.2f×"  (:speedup total))]))
  (println))

(defn -main [& _]
  (let [cfg {:f #(Math/sin (double %))
             :h 1.0e-2
             :xmin 0.0 :xmax 50.0
             :base-npts 100
             :append-npts 20
             :rounds 10}
        res (run-bench-append cfg)]
    (print-report res)))

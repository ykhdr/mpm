(ns lab2.part1.bench
  (:require [clojure.string :as str]))

(load-file "1.clj")

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

(defn gen-increasing-fixed [xmin xmax total-npts]
  (let [step (/ (- xmax xmin) (double (dec total-npts)))]
    (vec (map #(+ xmin (* step %)) (range total-npts)))))

(defn run-bench-append
  [{:keys [f h xmin xmax base-npts append-npts rounds]}]
  (let [total-npts (+ base-npts (* (max 0 (dec rounds)) append-npts))
        xs-full    (gen-increasing-fixed xmin xmax total-npts)
        xs-rounds  (mapv (fn [i]
                           (subvec xs-full 0 (+ base-npts (* i append-npts))))
                         (range rounds))
        F0   (fn [x] (trap-integral f h x))
        Fmem ((resolve 'lab2.part1/make-integral-op-memo) f h)

        naive-times
        (mapv (fn [xs]
                (let [t0 (now-ns)]
                  (doseq [x xs] (F0 x))
                  (- (double (now-ns)) t0)))
              xs-rounds)

        memo-times
        (mapv (fn [xs]
                (let [t0 (now-ns)]
                  (doseq [x xs] (Fmem x))
                  (- (double (now-ns)) t0)))
              xs-rounds)

        sum-naive (reduce + naive-times)
        sum-memo  (reduce + memo-times)]
    {:params {:h h :xmin xmin :xmax xmax
              :base-npts base-npts :append-npts append-npts :rounds rounds}
     :per-round
     (mapv (fn [i nt mt]
             {:round (inc i)
              :points (+ base-npts (* i append-npts))
              :added  (if (zero? i) 0 append-npts)
              :naive_ms (ms nt)
              :memo_ms  (ms mt)})
           (range rounds) naive-times memo-times)
     :total {:naive_ms (ms sum-naive)
             :memo_ms  (ms sum-memo)
             :speedup  (double (/ sum-naive sum-memo))}}))

(defn print-report [{:keys [params per-round total]}]
  (println (format "Incremental benchmark:"))
  (println (format "  range      = [%.1f, %.1f]" (:xmin params) (:xmax params)))
  (println (format "  h          = %g" (:h params)))
  (println (format "  base-npts  = %d" (:base-npts params)))
  (println (format "  append     = +%d per round" (:append-npts params)))
  (println (format "  rounds     = %d" (:rounds params)))
  (println "\nPer-round timing (ms):")
  (doseq [{:keys [round points added naive_ms memo_ms]} per-round]
    (println (format "  round %-2d  points=%-6d (+%d)   naive=%8s   memo=%8s"
                     round points added (fmt-ms naive_ms) (fmt-ms memo_ms))))
  (println "\nTotal:")
  (println (format "  naive   = %s ms" (fmt-ms (:naive_ms total))))
  (println (format "  mem     = %s ms" (fmt-ms (:memo_ms total))))
  (println (format "  speedup = %.2f×" (:speedup total))))

(defn -main [& _]
  (let [cfg {:f #(Math/sin (double %))
             :h 1.0e-3
             :xmin 0.0 :xmax 50.0
             :base-npts 100
             :append-npts 40
             :rounds 10}
        res (run-bench-append cfg)]
    (print-report res)))

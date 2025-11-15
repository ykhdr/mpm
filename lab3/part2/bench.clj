(ns lab3.part2.bench
  (:require [lab3.part2 :as P]))

(defn now-ns
  "Текущее время в наносекундах."
  []
  (System/nanoTime))

(defn ms
  "Перевод наносекунд в миллисекунды (double)."
  [ns]
  (/ ns 1.0e6))

(defn busy-pred
  "Строит тяжёлый по CPU предикат по параметру work."
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

(defn time-chunks
  "Запускает make-seq, затем по очереди берёт chunks батчей размера batch-size
   из полученной последовательности. На каждый батч делается split-at + doall,
   время на батч в мс записывается в вектор. Возвращает:
   {:create-ms <время создания ленивой последовательности>
    :chunk-ms  [t1 t2 ... tn]
    :total-ms  <суммарное время по всем батчам>}"
  [make-seq batch-size chunks]
  (let [t0 (now-ns)
        s0 (make-seq)
        t1 (now-ns)
        create-ms (ms (- t1 t0))]
    (println (format "%s created seq in %.3f ms"
                     (java.time.LocalTime/now) create-ms))
    (loop [i 1
           s s0
           acc []
           total 0.0]
      (if (> i chunks)
        {:create-ms create-ms
         :chunk-ms  acc
         :total-ms  total}
        (let [t-start (now-ns)
              [chunk s'] (split-at batch-size s)
              _          (doall chunk)
              t-end      (now-ns)
              dt         (ms (- t-end t-start))]
          (println (format "%s chunk %d: size=%d, dt=%.3f ms"
                           (java.time.LocalTime/now)
                           i (count chunk) dt))
          (recur (inc i) s' (conj acc dt) (+ total dt)))))))

(defn run-bench
  "cfg:
   {:batch-size  размер батча (сколько элементов за один 'шаг' потребляем)
    :chunks      сколько батчей подряд берём
    :work        нагрузка предиката
    :block       размер блока для pfilter
    :inflight    максимальное число параллельных блоков}"

  [{:keys [batch-size chunks work block inflight]}]
  (let [pred (busy-pred work)
        batch (long batch-size)
        nch   (long chunks)]
    (println "=== filter (однопоточный, ленивый) ===")
    (let [res-f (time-chunks #(filter pred (range)) batch nch)]
      (println "=== pfilter (параллельный, ленивый) ===")
      (let [res-p (time-chunks #(P/pfilter pred (range) block inflight) batch nch)]
        {:cfg {:batch-size batch
               :chunks     nch
               :work       work
               :block      block
               :inflight   inflight}
         :filter res-f
         :pfilter res-p}))))

(defn -main
  "Запускать, например: clj -M:bench-part2"
  [& _]
  (let [cfg {:batch-size 2000
             :chunks     5
             :work       20000
             :block      500
             :inflight   (.availableProcessors (Runtime/getRuntime))}
        {:keys [cfg filter pfilter]} (run-bench cfg)]
    (println "\n=== SUMMARY ===")
    (println "Config:" cfg)
    (println (format "filter  : create=%.3f ms, total-chunks=%.3f ms, per-chunk=%s"
                     (:create-ms filter)
                     (:total-ms filter)
                     (pr-str (:chunk-ms filter))))
    (println (format "pfilter : create=%.3f ms, total-chunks=%.3f ms, per-chunk=%s"
                     (:create-ms pfilter)
                     (:total-ms pfilter)
                     (pr-str (:chunk-ms pfilter))))
    (println (format "speedup (total chunks time): %.2fx"
                     (/ (:total-ms filter) (:total-ms pfilter))))))

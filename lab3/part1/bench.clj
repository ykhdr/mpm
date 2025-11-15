(ns lab3.part1.bench
  (:require [lab3.part1 :as P]))

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

(defn time-chunks-lazy
  "measure-seq: () -> ленивый seq.
   Берёт подряд chunks батчей batch-size, для каждого batсha считает doall.
   Возвращает {:create-ms :chunk-ms :total-ms}."
  [measure-seq batch-size chunks]
  (let [t0 (now-ns)
        s0 (measure-seq)
        t1 (now-ns)
        create-ms (ms (- t1 t0))]
    (println (format "%s lazy seq created in %.3f ms"
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
          (println (format "%s lazy chunk %d: size=%d, dt=%.3f ms"
                           (java.time.LocalTime/now)
                           i (count chunk) dt))
          (recur (inc i) s' (conj acc dt) (+ total dt)))))))

(defn time-chunks-strict
  "measure-coll: () -> уже посчитанная коллекция (vector).
   Меряем время её создания, потом режем на chunks батчей batch-size через subvec
   и для каждого батча делаем простую агрегацию, чтобы не выкинуть работу JIT.
   Возвращает {:create-ms :chunk-ms :total-ms}."
  [measure-coll batch-size chunks]
  (let [t0 (now-ns)
        coll (measure-coll)
        t1 (now-ns)
        create-ms (ms (- t1 t0))
        n (count coll)]
    (println (format "%s strict coll created in %.3f ms (size=%d)"
                     (java.time.LocalTime/now) create-ms n))
    (loop [i 1
           idx 0
           acc []
           total 0.0]
      (if (or (> i chunks) (>= idx n))
        {:create-ms create-ms
         :chunk-ms  acc
         :total-ms  total}
        (let [end (min n (+ idx batch-size))
              t-start (now-ns)
              chunk  (subvec coll idx end)
              _      (reduce + 0 chunk)
              t-end  (now-ns)
              dt     (ms (- t-end t-start))]
          (println (format "%s strict chunk %d: size=%d, dt=%.3f ms"
                           (java.time.LocalTime/now)
                           i (count chunk) dt))
          (recur (inc i) end (conj acc dt) (+ total dt)))))))

(defn run-bench
  "cfg:
   {:total      общее количество исходных элементов
    :batch-size размер батча при чтении результата
    :chunks     сколько батчей читаем
    :work       нагрузка предиката
    :block      размер блока для pfilter}"
  [{:keys [total batch-size chunks work block]}]
  (let [total (long total)
        batch (long batch-size)
        nch   (long chunks)
        pred  (busy-pred work)
        coll  (vec (range total))]
    (println "=== filter (lazy) over finite vector ===")
    (let [res-f (time-chunks-lazy #(filter pred coll) batch nch)]
      (println "=== pfilter (strict) over finite vector ===")
      (let [res-p (time-chunks-strict #(P/pfilter pred coll block) batch nch)]
        {:cfg     {:total total
                   :batch-size batch
                   :chunks nch
                   :work work
                   :block block}
         :filter  res-f
         :pfilter res-p}))))

(defn -main
  "Запускать, например: clj -M:bench-part1-stream"
  [& _]
  (let [cfg {:total      100000
             :batch-size 5000
             :chunks     5
             :work       20000
             :block      1000}
        {:keys [cfg filter pfilter]} (run-bench cfg)]
    (println "\n=== SUMMARY (part1 strict vs lazy filter) ===")
    (println "Config:" cfg)
    (println (format "filter  : create=%.3f ms, total-chunks=%.3f ms, per-chunk=%s"
                     (:create-ms filter)
                     (:total-ms filter)
                     (pr-str (:chunk-ms filter))))
    (println (format "pfilter : create=%.3f ms, total-chunks=%.3f ms, per-chunk=%s"
                     (:create-ms pfilter)
                     (:total-ms pfilter)
                     (pr-str (:chunk-ms pfilter))))
    (println (format "speedup (total chunks time, включая создание): %.2fx"
                     (/ (+ (:create-ms filter)  (:total-ms filter))
                        (+ (:create-ms pfilter) (:total-ms pfilter)))))))

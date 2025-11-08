(ns lab2.part1)

(defn trap-integral
  "Численный интеграл ∫_0^x f(t) dt методом трапеций с постоянным шагом h"
  [f h x]
  (let [h (double h)
        sign (if (neg? x) -1.0 1.0)
        X (Math/abs (double x))
        n (long (Math/floor (/ X h)))
        r (- X (* n h))]
    (loop [k 0
           acc 0.0
           prev (f 0.0)]
      (if (< k n)
        (let [t1 (* (inc k) h)
              v1 (f t1)
              area (* 0.5 h (+ prev v1))]
          (recur (inc k) (+ acc area) v1))
        (let [vn (if (zero? n) (f 0.0) prev)
              vx (f X)
              last-area (* 0.5 r (+ vn vx))]
          (* sign (+ acc last-area)))))))

(defn make-integral-op
  "Возвращает F(x) = ∫_0^x f(t) dt"
  [f h]
  (fn [x] (trap-integral f h x)))

(defn make-integral-op-memo
  "Мемоизированная версия: кэширует значения F(x) для аргументов x"
  [f h]
  (let [base (make-integral-op f h)
        memoF (memoize base)]
    (fn [x] (memoF x))))

(defn -main [& _]
  (let [f #(Math/sin (double %))
        h 1.0e-4
        F (make-integral-op-memo f h)]
    (doseq [x [0.0 1.0 Math/PI]]
      (println "F(" x ")=" (F x)))))

(ns part2-test
  (:require [clojure.test :refer :all]))

(load-file "2.clj")
(require 'lab3.part2)
(alias 'P 'lab3.part2)

(defn bounded-range
  "Ленивая последовательность 0..limit-1.
   При попытке прочитать элемент с индексом == limit кидает исключение.
   Удобно, чтобы проверять, что pfilter не уходит слишком далеко вперёд."
  [^long limit]
  (letfn [(step [^long i]
            (lazy-seq
             (if (< i limit)
               (cons i (step (inc i)))
               (throw (ex-info "over-read" {:i i :limit limit})))))]
    (step 0)))

(defn tapping-range
  "Возвращает [seq hits-atom], где hits-atom накапливает просмотренные элементы.
   Полезно, чтобы убедиться, что до first/take ничего не читается."
  [^long n]
  (let [hits (atom [])]
    [(map (fn [x] (swap! hits conj x) x) (range n)) hits]))

(deftest correctness-finite-matrix
  (let [xs   (vec (range 1000))
        pred #(and (odd? %) (zero? (mod % 3)))]
    (doseq [block [1 2 3 7 64 256 2000]
            infl  [1 2 4 8]]
      (is (= (vec (filter pred xs))
             (vec (P/pfilter pred xs block infl)))
          (format "mismatch for block=%d inflight=%d" block infl)))))

(deftest order-is-preserved
  (let [xs   (range 37)
        pred #(or (= % 0) (odd? %))]
    (is (= (vec (filter pred xs))
           (vec (P/pfilter pred xs 5 3))))))

(deftest empty-result-finite
  (let [xs   (range 100)
        pred (constantly false)]
    (is (= []
           (vec (P/pfilter pred xs 8 4))))))

(deftest lazy-no-preload-before-consumption
  (let [[src hits] (tapping-range 100)
        s          (P/pfilter odd? src 10 2)]
    ;; пока ничего не запросили — вход не трогали
    (is (empty? @hits))
    ;; первый элемент — уже есть какая-то активность, но это допускается;
    ;; главное — до first вход «не смотрели»
    (is (= 1 (first s)))
    (is (not (empty? @hits)))))

(deftest laziness-with-infinite-source
  (let [pred odd?
        s1   (take 20 (filter  pred (range)))
        s2   (take 20 (P/pfilter pred (range) 16 3))]
    (is (= (vec s1) (vec s2)))))

(deftest bounded-forward-read-with-true-pred
  ;; Проверяем верхнюю границу «забега вперёд», когда pred всегда истина.
  ;; Для выдачи N элементов нужно B=ceil(N/block) чанков.
  ;; P сначала загружает p чанков, потом на каждый следующий отданный чанк подкачивает ещё один.
  ;; Безопасный лимит на вход: limit = (p + B) * block.
  (let [N     100
        block 16
        p     3
        B     (long (Math/ceil (/ (double N) block)))
        limit (* (+ p B) block)
        src   (bounded-range limit)
        res   (doall (take N (P/pfilter (constantly true) src block p)))]
    (is (= N (count res)))))

(deftest exception-propagation
  (let [xs   (range 100)
        pred (fn [x]
               (if (= x 42)
                 (throw (ex-info "boom" {:x x}))
                 (odd? x)))]
    (try
      (doall (P/pfilter pred xs 5 2))
      (is false "expected exception")
      (catch clojure.lang.ExceptionInfo e
        (is (= "boom" (.getMessage e)))
        (is (= 42 (:x (ex-data e)))))
      (catch java.util.concurrent.ExecutionException e
        (let [c (.getCause e)]
          (is (instance? clojure.lang.ExceptionInfo c))
          (is (= "boom" (.getMessage ^Throwable c)))
          (is (= 42 (:x (ex-data c)))))))))

;; Пограничные параметры inflight/block
(deftest edge-params
  (let [xs   (range 50)
        pred odd?]
    (is (= (vec (filter pred xs))
           (vec (P/pfilter pred xs 1 1))))
    (is (= (vec (filter pred xs))
           (vec (P/pfilter pred xs 50 1))))
    (is (= (vec (filter pred xs))
           (vec (P/pfilter pred xs 7 64))))))

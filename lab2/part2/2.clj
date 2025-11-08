(ns lab2.part2)

(defn make-integral-op-stream
  [f h]
  (let [step  (double h)
        ts    (iterate #(+ % step) 0.0)
        vs0   (map #(double (f %)) ts)
        areas (map #(* 0.5 step (+ %1 %2)) vs0 (rest vs0))
        sums0 (reductions + 0.0 areas)
        state (object-array 1)]
    (aset state 0 {:k 0 :vs vs0 :sums sums0})
    (fn [x]
      (let [sign  (if (neg? x) -1.0 1.0)
            X     (Math/abs (double x))
            k-new (int (Math/floor (/ X step)))
            r     (- X (* k-new step))
            {:keys [k vs sums]} (aget state 0)
            [vs' sums']
            (loop [i k v vs s sums]
              (if (< i k-new) (recur (inc i) (rest v) (rest s)) [v s]))]
        (aset state 0 {:k k-new :vs vs' :sums sums'})
        (let [Sk (first sums')
              vk (first vs')
              vx (double (f X))]
          (* sign (+ Sk (* 0.5 r (+ vk vx)))))))))

(defn -main [& _]
  (let [f #(Math/sin (double %))
        h 1.0e-4
        F (make-integral-op-stream f h)]
    (doseq [x [0.0 1.0 10.0 20.0 35.0 50.0]]
      (println "F(" x ")=" (F x)))))

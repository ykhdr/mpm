(ns lab3.part2)

(defn chunk-blocks
  "Лениво разбивает `coll` на блоки по `block-size` элементов"
  [coll block-size]
  (when (<= (long block-size) 0)
    (throw (ex-info "block-size must be positive" {:block-size block-size})))
  (letfn [(step [xs]
            (lazy-seq
             (when-let [s (seq xs)]
               (let [blk (vec (take block-size s))]
                 (cons blk (step (drop block-size s)))))))]
    (step coll)))

(defn pfilter
  "Ленивый параллельный фильтр по чанкам через `future`.
   Сохраняет порядок и работает с бесконечными последовательностями."
  ([pred coll block-size]
   (pfilter pred coll block-size (.availableProcessors (Runtime/getRuntime))))
  ([pred coll block-size inflight]
   (let [n (long block-size)
         p (long inflight)]
     (when (<= n 0) (throw (ex-info "block-size must be positive" {:block-size block-size})))
     (when (<= p 0) (throw (ex-info "inflight must be positive" {:inflight inflight})))
     (let [chunks (chunk-blocks coll n)]
       (letfn [(fill [chs futs]
                 (loop [c chs, fs futs]
                   (if (and (< (count fs) p) (seq c))
                     (recur (rest c)
                            (conj fs (future (vec (filter pred (first c))))))
                     [c fs])))
               (produce [chs futs]
                 (lazy-seq
                  (let [[chs' fs'] (fill chs futs)]
                    (when (seq fs')
                      (let [batch @(nth fs' 0)
                            fs''  (subvec fs' 1)]
                        (if (seq batch)
                          (concat batch (produce chs' fs''))
                          (produce chs' fs'')))))))]
         (produce chunks []))))))

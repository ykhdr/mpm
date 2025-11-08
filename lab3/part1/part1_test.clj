(ns part1-test
  (:require [clojure.test :refer :all]))
(load-file "1.clj")
(require 'lab3.part1)
(alias 'P 'lab3.part1)

(deftest pfilter-correctness
  (let [xs (range 1000)
        pred odd?]
    (is (= (vec (filter pred xs))
           (P/pfilter pred xs 7)))))

(deftest chunk-basic
  (is (= (P/chunk-blocks (range 0) 5) []))
  (is (= (P/chunk-blocks (range 1 6) 2) [[1 2] [3 4] [5]]))
  (is (= (P/chunk-blocks [1 2 3 4 5 6] 3) [[1 2 3] [4 5 6]])))

(deftest invalid-block
  (is (thrown? Exception (P/chunk-blocks (range 10) 0))))

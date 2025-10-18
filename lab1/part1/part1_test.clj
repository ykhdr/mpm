(ns part1-test
  (:require [clojure.test :refer :all]))

(load-file "1.clj")

(deftest edge-cases
  (is (= '()  (lab1.part1/strings-no-adjacent-rec "abc" -1)))
  (is (= '("") (lab1.part1/strings-no-adjacent-rec "abc" 0)))
  (is (= '()  (lab1.part1/strings-no-adjacent-rec "" 2))))

(deftest sample-n2
  (let [res (set (lab1.part1/strings-no-adjacent-rec "abc" 2))]
    (is (= res #{"ab" "ac" "ba" "bc" "ca" "cb"}))
    (is (= 6 (count res)))))

(deftest duplicates-in-alphabet
  (is (= (set (lab1.part1/strings-no-adjacent-rec "aab" 2))
         #{"ab" "ba"}))
  (is (= (set (lab1.part1/strings-no-adjacent-rec "ababa" 2))
         #{"ab" "ba"})))

(deftest length-and-adjacent
  (doseq [chars ["ab" "abcd" "xyz" "aab"]
          n     [1 2 3]]
    (let [out (lab1.part1/strings-no-adjacent-rec chars n)]
      (is (every? #(= (count %) n) out))
      (is (every? (fn [s]
                    (or (<= (count s) 1)
                        (every? (fn [[a b]] (not= a b))
                                (partition 2 1 s))))
                  out)))))

(deftest combinatorics-count
  (doseq [chars ["ab" "abcd" "qwe" "aab"]
          n     [0 1 2 3]]
    (let [k (count (lab1.part1/distinct-preserving-order (seq chars)))
          expected (cond
                     (neg? n) 0
                     (zero? n) 1
                     :else (long (* k (Math/pow (max 0 (dec k)) (dec n)))))
          actual (count (lab1.part1/strings-no-adjacent-rec chars n))]
      (is (= expected actual)))))

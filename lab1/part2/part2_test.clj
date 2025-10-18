(ns part2-test
  (:require [clojure.test :refer :all]))

(load-file "2.clj")

(deftest edge-cases
  (is (= '()  (lab1.part2/strings-no-adjacent-tail "abc" -1)))
  (is (= '("") (lab1.part2/strings-no-adjacent-tail "abc" 0)))
  (is (= '()  (lab1.part2/strings-no-adjacent-tail "" 2))))

(deftest sample-n2
  (let [res (set (lab1.part2/strings-no-adjacent-tail "abc" 2))]
    (is (= res #{"ab" "ac" "ba" "bc" "ca" "cb"}))
    (is (= 6 (count res)))))

(deftest duplicates-in-alphabet
  (is (= (set (lab1.part2/strings-no-adjacent-tail "aab" 2))
         #{"ab" "ba"}))
  (is (= (set (lab1.part2/strings-no-adjacent-tail "ababa" 2))
         #{"ab" "ba"})))

(deftest length-and-adjacent
  (doseq [chars ["ab" "abcd" "xyz" "aab"]
          n     [1 2 3]]
    (let [out (lab1.part2/strings-no-adjacent-tail chars n)]
      (is (every? #(= (count %) n) out))
      (is (every? (fn [s]
                    (or (<= (count s) 1)
                        (every? (fn [[a b]] (not= a b))
                                (partition 2 1 s))))
                  out)))))

(deftest combinatorics-count-unique
  (doseq [chars ["ab" "abcd" "qwe" "aab"]
          n     [0 1 2 3]]
    (let [k (count (lab1.part2/distinct-preserving-order (seq chars)))
          expected (cond
                     (neg? n) 0
                     (zero? n) 1
                     :else (long (* k (Math/pow (max 0 (dec k)) (dec n)))))
          actual (count (lab1.part2/strings-no-adjacent-tail chars n))]
      (is (= expected actual)))))

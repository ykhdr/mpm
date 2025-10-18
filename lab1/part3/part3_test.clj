(ns part3-test
  (:require [clojure.test :refer :all]))

(load-file "3.clj")  ;; загружает ns lab1.part3 из файла 3.clj

(deftest my-map-vs-core-map
  (is (= (lab1.part3/my-map inc '())             (map inc '())))
  (is (= (lab1.part3/my-map inc '(1))            (map inc '(1))))
  (is (= (lab1.part3/my-map inc '(1 2 3))        (map inc '(1 2 3))))
  (is (= (lab1.part3/my-map #(* % %) '(1 2 3 4)) (map #(* % %) '(1 2 3 4))))
  (is (= (lab1.part3/my-map inc [1 2 3])         (map inc [1 2 3]))))

(deftest my-filter-vs-core-filter
  (is (= (lab1.part3/my-filter odd? '())                (filter odd? '())))
  (is (= (lab1.part3/my-filter odd? '(1))               (filter odd? '(1))))
  (is (= (lab1.part3/my-filter odd? '(1 2 3 4))         (filter odd? '(1 2 3 4))))
  (is (= (lab1.part3/my-filter #(> % 2) '(1 2 3 4 5))   (filter #(> % 2) '(1 2 3 4 5))))
  (is (= (lab1.part3/my-filter odd? [1 2 3 4])          (filter odd? [1 2 3 4]))))

(deftest order-preserved
  (is (= (lab1.part3/my-map identity '(a b c)) '(a b c)))
  (is (= (lab1.part3/my-filter (constantly true) '(a b c)) '(a b c)))
  (is (= (lab1.part3/my-filter (constantly false) '(a b c)) '())))

(deftest nil-and-empty-safe
  (is (= (lab1.part3/my-map inc nil) '()))
  (is (= (lab1.part3/my-filter odd? nil) '())))

;; --- strings-no-adjacent-hofs (1.4) ---

(deftest sample-n2
  (let [res (set (lab1.part3/strings-no-adjacent-hofs "abc" 2))]
    (is (= res #{"ab" "ac" "ba" "bc" "ca" "cb"}))
    (is (= 6 (count res)))))

(deftest duplicates-in-alphabet
  (is (= (set (lab1.part3/strings-no-adjacent-hofs "aab" 2))
         #{"ab" "ba"}))
  (is (= (set (lab1.part3/strings-no-adjacent-hofs "ababa" 2))
         #{"ab" "ba"})))

(deftest length-and-adjacent
  (doseq [chars ["ab" "abcd" "xyz" "aab"]
          n     [1 2 3]]
    (let [out (lab1.part3/strings-no-adjacent-hofs chars n)]
      (is (every? #(= (count %) n) out))
      (is (every? (fn [s]
                    (or (<= (count s) 1)
                        (every? (fn [[a b]] (not= a b))
                                (partition 2 1 s))))
                  out)))))

(deftest combinatorics-count-unique
  (doseq [chars ["ab" "abcd" "qwe" "aab"]
          n     [0 1 2 3]]
    (let [k (count (lab1.part3/distinct-preserving-order (seq chars)))
          expected (cond
                     (neg? n) 0
                     (zero? n) 1
                     :else (long (* k (Math/pow (max 0 (dec k)) (dec n)))))
          actual (count (lab1.part3/strings-no-adjacent-hofs chars n))]
      (is (= expected actual)))))

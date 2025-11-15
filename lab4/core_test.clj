(ns core-test
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [core :as d]))

; Вспомогательные функции для тестов

(defn all-assignments
  "Все возможные окружения для множества переменных vars.
   Возвращает последовательность map-ов вида {A true, B false, ...}"
  [vars]
  (if (empty? vars)
    [{}]
    (let [[v & vs] (seq vars)
          tail-envs (all-assignments (set vs))]
      (concat
       (for [m tail-envs] (assoc m v false))
       (for [m tail-envs] (assoc m v true))))))

(defn equivalent?
  "Проверить логическую эквивалентность двух выражений e1 и e2.
   Сравнение производится по всем наборам значений переменных"
  [e1 e2]
  (let [vars (set/union (d/vars-of e1) (d/vars-of e2))]
    (every?
     (fn [env]
       (= (d/eval-expr e1 env)
          (d/eval-expr e2 env)))
     (all-assignments vars))))

; Тесты

(deftest constructors-test
  (testing "VAR/BOOL/NOT/AND/OR/IMP конструируют правильные AST"
    (is (= 'A (d/VAR 'A)))
    (is (= true (d/BOOL 1)))
    (is (= false (d/BOOL nil)))
    (is (= {:op :not  :args ['A]} (d/NOT 'A)))
    (is (= {:op :and  :args ['A 'B]} (d/AND 'A 'B)))
    (is (= {:op :or   :args ['A 'B 'C]} (d/OR 'A 'B 'C)))
    (is (= {:op :imp  :args ['A 'B]} (d/IMP 'A 'B)))
    (is (= {:op :xor  :args ['A 'B]} (d/XOR 'A 'B)))
    (is (= {:op :nor  :args ['A 'B]} (d/NOR 'A 'B)))
    (is (= {:op :nand :args ['A 'B]} (d/NAND 'A 'B)))))

(deftest desugar-imp-test
  (testing "desugar импликации A -> B"
    (let [expr (d/IMP 'A 'B)
          des  (d/desugar expr)]
      (is (= des
             (d/OR (d/NOT 'A) 'B)))
      (is (equivalent? expr des)))))

(deftest desugar-derived-test
  (testing "desugar для производных операций XOR/NOR/NAND"
    (let [xor-expr  (d/XOR 'A 'B)
          nor-expr  (d/NOR 'A 'B)
          nand-expr (d/NAND 'A 'B)]
      ; должно не содержать :xor/:nor/:nand после desugar
      (doseq [e [xor-expr nor-expr nand-expr]]
        (let [des (d/desugar e)]
          (is (nil? (some #{:xor :nor :nand}
                          (tree-seq d/op-node? d/args-of des)))))))))

; Тесты упрощения и NNF/ДНФ

(deftest simplify-basic-test
  (testing "упрощение констант и двойного отрицания"
    (is (= true  (d/simplify (d/AND true))))
    (is (= false (d/simplify (d/OR false false))))
    (is (= 'A    (d/simplify (d/NOT (d/NOT 'A)))))
    (is (= false (d/simplify (d/AND 'A false 'B))))
    (is (= true  (d/simplify (d/OR 'A true 'B))))))

(deftest nnf-demorgan-test
  (testing "NNF и законы де Моргана"
    (let [expr    (d/NOT (d/AND 'A (d/OR 'B 'C)))
          nnf-ex  (d/nnf expr)
          expected (d/OR (d/NOT 'A)
                         (d/AND (d/NOT 'B) (d/NOT 'C)))]
      (is (equivalent? expr nnf-ex))
      (is (equivalent? expected nnf-ex)))))

(deftest to-dnf-distributive-test
  (testing "классический пример: A & (B | C) -> (A & B) | (A & C)"
    (let [expr (d/AND 'A (d/OR 'B 'C))
          dnf  (d/to-dnf expr)
          expected (d/OR (d/AND 'A 'B)
                         (d/AND 'A 'C))]
      (is (d/dnf? dnf))
      (is (equivalent? expr dnf))
      (is (equivalent? expected dnf)))))

(deftest to-dnf-imp-xor-test
  (testing "to-dnf для формул с импликацией и XOR/NOR/NAND"
    (let [exprs [(d/IMP 'A 'B)
                 (d/XOR 'A 'B)
                 (d/NOR 'A 'B)
                 (d/NAND 'A 'B)
                 (d/AND (d/XOR 'A 'B) (d/OR 'C 'D))
                 (d/OR  (d/NOR 'A 'B) (d/NAND 'C 'D))]]
      (doseq [e exprs]
        (let [dnf (d/to-dnf e)]
          (is (d/dnf? dnf))
          (is (equivalent? e dnf)))))))

; Тест подстановки

(deftest substitute-to-dnf-test
  (testing "substitute->dnf корректно подставляет и упрощает"
    (let [expr (d/OR 'A (d/AND 'B (d/NOT 'C)))
          dnf  (d/substitute->dnf expr {'A true})]
      (is (= true dnf)))

    (let [expr (d/IMP 'A 'B)
          dnf  (d/substitute->dnf expr {'A true 'B false})]
      (is (= false dnf)))

    (let [expr (d/XOR 'A 'B)
          dnf  (d/substitute->dnf expr {'A false})]
      (is (equivalent? 'B dnf))
      (is (d/dnf? dnf)))))

(def small-exprs
  "Набор небольших тестовых выражений для проверки to-dnf"
  [(d/VAR 'A)
   (d/NOT (d/VAR 'A))
   (d/AND 'A 'B)
   (d/OR 'A 'B)
   (d/IMP 'A 'B)
   (d/AND (d/OR 'A 'B) 'C)
   (d/OR  (d/AND 'A 'B) 'C)
   (d/XOR 'A 'B)
   (d/NOR (d/OR 'A 'B) 'C)
   (d/NAND (d/AND 'A 'B) (d/NOT 'C))])

(deftest equivalence-random-small
  (testing "expr и to-dnf expr логически эквивалентны на малых формулах"
    (doseq [e small-exprs]
      (let [dnf (d/to-dnf e)]
        (is (d/dnf? dnf))
        (is (equivalent? e dnf))))))

(deftest to-dnf-many-expressions-test
  (testing "Набор проверок to-dnf на разных формулах"
    (let [cases
          [;; 1. Переменная
           {:name "var-A"
            :expr (d/VAR 'A)
            :expected (d/VAR 'A)}

           ;; 2. Двойное отрицание
           {:name "double-negation"
            :expr (d/NOT (d/NOT 'A))
            :expected (d/VAR 'A)}

           ;; 3. Дистрибутивность: A & (B | C)
           ;;    ожидаем (A & B) | (A & C)
           {:name "and-distributes-over-or"
            :expr (d/AND 'A (d/OR 'B 'C))
            :expected (d/OR (d/AND 'A 'B)
                            (d/AND 'A 'C))}

           ;; 4. A | (B & C) уже в ДНФ
           {:name "or-with-and"
            :expr (d/OR 'A (d/AND 'B 'C))
            :expected (d/OR 'A (d/AND 'B 'C))}

           ;; 5. Импликация: A -> B = !A | B
           {:name "implication-A->B"
            :expr (d/IMP 'A 'B)
            :expected (d/OR (d/NOT 'A) 'B)}

           ;; 6. XOR(A,B) = (A & !B) | (!A & B)
           {:name "xor-A-B"
            :expr (d/XOR 'A 'B)
            :expected (d/OR (d/AND 'A (d/NOT 'B))
                            (d/AND (d/NOT 'A) 'B))}

           ;; 7. NOR(A,B) = !(A | B) = !A & !B
           {:name "nor-A-B"
            :expr (d/NOR 'A 'B)
            :expected (d/AND (d/NOT 'A)
                             (d/NOT 'B))}

           ;; 8. NAND(A,B) = !(A & B) = !A | !B
           {:name "nand-A-B"
            :expr (d/NAND 'A 'B)
            :expected (d/OR (d/NOT 'A)
                            (d/NOT 'B))}

           ;; 9. Тавтология: A | true => true
           {:name "tautology-A-or-true"
            :expr (d/OR 'A true)
            :expected true}

           ;; 10. Противоречие: A & !A => false
           {:name "contradiction-A-and-not-A"
            :expr (d/AND 'A (d/NOT 'A))
            :expected false}

            ;; 11. (A & (B | C)) | (!A & B)
            ;;     → (A & B) | (A & C) | (!A & B)
           {:name "complex-and-or-chain"
            :expr (d/OR (d/AND 'A (d/OR 'B 'C))
                        (d/AND (d/NOT 'A) 'B))
            :expected (d/OR (d/AND 'A 'B)
                            (d/AND 'A 'C)
                            (d/AND (d/NOT 'A) 'B))}

            ;; 12. (A -> B) & (B -> C)
            ;;     A->B = !A | B
            ;;     B->C = !B | C
            ;;     (!A | B) & (!B | C)
            ;;       → (!A & !B) | (!A & C) | (B & !B) | (B & C)
            ;;       → (!A & !B) | (!A & C) | (B & C)
           {:name "complex-imp-chain"
            :expr (d/AND (d/IMP 'A 'B)
                         (d/IMP 'B 'C))
            :expected (d/OR (d/AND (d/NOT 'A) (d/NOT 'B))
                            (d/AND (d/NOT 'A) 'C)
                            (d/AND 'B 'C))}

            ;; 13. (XOR(A,B) & C) | NAND(B,C)
            ;;     XOR(A,B) = (A & !B) | (!A & B)
            ;;     NAND(B,C) = !B | !C
            ;;     → (A & !B & C) | (!A & B & C) | !B | !C
           {:name "complex-xor-nand"
            :expr (d/OR (d/AND (d/XOR 'A 'B) 'C)
                        (d/NAND 'B 'C))
            :expected (d/OR (d/AND 'A (d/NOT 'B) 'C)
                            (d/AND (d/NOT 'A) 'B 'C)
                            (d/NOT 'B)
                            (d/NOT 'C))}

            ;; 14. (A -> (B | C)) | XOR(B,C)
            ;;     A -> (B | C) = !A | B | C
            ;;     XOR(B,C) = (B & !C) | (!B & C)
            ;;     → !A | B | C | (B & !C) | (!B & C)
           {:name "complex-imp-xor-mix"
            :expr (d/OR (d/IMP 'A (d/OR 'B 'C))
                        (d/XOR 'B 'C))
            :expected (d/OR (d/NOT 'A)
                            'B
                            'C
                            (d/AND 'B (d/NOT 'C))
                            (d/AND (d/NOT 'B) 'C))}]]

      (doseq [{:keys [name expr expected]} cases
              :let [dnf (d/to-dnf expr)]]

        ;; 1) результат обязан быть в ДНФ
        (is (d/dnf? dnf)
            (str name ": результат to-dnf должен быть в ДНФ"))

        ;; 2) логическая эквивалентность исходника и ДНФ
        (is (equivalent? expr dnf)
            (str name ": исходное выражение и его ДНФ должны быть логически эквивалентны"))

        ;; 3) точное совпадение с ожидаемой формой
        (is (= expected dnf)
            (str name ": ДНФ-форма отличается от ожидаемой"))))))

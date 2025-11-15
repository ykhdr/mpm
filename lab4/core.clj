(ns core
  "Символьные булевы выражения и приведение к ДНФ"
  (:require [clojure.set :as set]))

;;  Представление AST

(defn VAR
  "Создать переменную (символ). На самом деле просто возвращает sym.
   Пример: (VAR 'A) => 'A"
  [sym]
  sym)

(defn BOOL
  "Создать булеву константу из произвольного значения.
   Пример: (BOOL 0) => false, (BOOL :x) => true"
  [b]
  (boolean b))

(defn NOT
  "Отрицание. AST-узел вида {:op :not, :args [x]}"
  [x]
  {:op :not :args [x]})

(defn AND
  "Конъюнкция. AST-узел вида {:op :and, :args [x1 x2 ...]}"
  [& xs]
  {:op :and :args (vec xs)})

(defn OR
  "Дизъюнкция. AST-узел вида {:op :or, :args [x1 x2 ...]}"
  [& xs]
  {:op :or :args (vec xs)})

(defn IMP
  "Импликация. AST-узел вида {:op :imp, :args [A B]} (A -> B)"
  [a b]
  {:op :imp :args [a b]})

;; Дополнительные операции

(defn XOR
  "Исключающее ИЛИ. Пока просто конструктор узла {:op :xor, :args [a b]}.
   Сама семантика задаётся через register-derived-op!."
  [a b]
  {:op :xor :args [a b]})

(defn NOR
  "Стрелка Пирса (not (A or B)): {:op :nor, :args [a b]}."
  [a b]
  {:op :nor :args [a b]})

(defn NAND
  "Штрих Шеффера (not (A and B)): {:op :nand, :args [a b]}."
  [a b]
  {:op :nand :args [a b]})

;; Вспомогательные функции для работы с AST

(defn op-node?
  "Проверить, что e — узел операции (map с ключом :op)"
  [e]
  (and (map? e) (contains? e :op)))

(defn op-of
  "Вернуть ключ операции из узла e (:and/:or/:not/...).
   Если не узел операции — вернуть nil"
  [e]
  (when (op-node? e)
    (:op e)))

(defn args-of
  "Вернуть вектор аргументов узла операции e.
   Если не узел — вернуть nil."
  [e]
  (when (op-node? e)
    (:args e)))

;; Регистрация производных операций

(def ^:private derived-reg
  "Реестр производных операций:
   * ключ — ключ операции (:xor, :nor, ...),
   * значение — функция (fn [args] <выражение в базовых :and/:or/:not>).

   Каждая такая функция должна построить выражение только из базовых операций/констант/переменных."
  (atom {}))

(defn register-derived-op!
  "Зарегистрировать преобразование производной операции op-key к базовым (and/or/not).

   lower-fn - функция (fn [args]) -> выражение.
   Пример:
     (register-derived-op! :xor
       (fn [[a b]]
         (OR (AND a (NOT b))
             (AND (NOT a) b))))"
  [op-key lower-fn]
  (swap! derived-reg assoc op-key lower-fn))

;; Зарегистрируем несколько типичных операций как примеры:

(register-derived-op!
 :xor
 (fn [[a b]]
   (OR (AND a (NOT b))
       (AND (NOT a) b))))

(register-derived-op!
 :nor
 (fn [[a b]]
   (NOT (OR a b))))

(register-derived-op!
 :nand
 (fn [[a b]]
   (NOT (AND a b))))

;; Понижение синтаксиса

(declare desugar simplify nnf nnf-neg)

(defn desugar
  "Понизить выражение e:
   * импликацию :imp -> через OR/NOT,
   * все операции из derived-reg -> выражения на базовых :and/:or/:not,
   * рекурсивно обрабатывает подвыражения.

   На выходе - выражение, содержащее только:
   - булевы константы,
   - переменные (symbol),
   - операции :and/:or/:not."
  [e]
  (cond
    (boolean? e) e
    (symbol? e)  e

    (op-node? e)
    (let [op (op-of e)
          as (map desugar (args-of e))]
      (case op
        :imp (desugar (OR (NOT (first as)) (second as)))
        :not (NOT (first as))
        :and (apply AND as)
        :or  (apply OR as)
        ;; производные операции
        (if-let [lower (@derived-reg op)]
          (desugar (lower as))
          (throw (ex-info "Unknown op" {:op op})))))

    :else
    (throw (ex-info "Bad expr" {:expr e}))))

;; Подстановка переменных

(defn subst
  "Подстановка окружения env (map symbol->boolean) в выражение e.
   Неприсутствующие в env символы остаются переменными.

   Пример: (subst (AND 'A (NOT 'B)) {'A true}) => {:op :and, :args [true {:op :not, :args ['B]}]}"
  [e env]
  (cond
    (boolean? e) e
    (symbol? e)  (if (contains? env e) (BOOL (env e)) e)
    (op-node? e)
    (let [as (map #(subst % env) (args-of e))]
      {:op (op-of e)
       :args (vec as)})

    :else
    (throw (ex-info "Bad expr" {:expr e}))))

;; Упрощение выражений

(defn flatten-op
  "Делаем плоскими вложенные однотипные операции: (and A (and B C)) => (A B C)"
  [op-key xs]
  (mapcat
   (fn [x]
     (if (and (op-node? x) (= (op-of x) op-key))
       (args-of x)
       [x]))
   xs))

(defn simplify
  "Локальные алгебраические упрощения:
   * константы,
   * двойное отрицание !!X,
   * ассоциативность (расплющивание вложенных and/or),
   * удаление дубликатов,
   * удаление нейтральных элементов (true в and, false в or),
   * короткие формы and/or с 0 или 1 аргументом.
   Также понижает :imp и производные операции"
  [e]
  (cond
    (boolean? e) e
    (symbol? e)  e

    (op-node? e)
    (let [op (op-of e)
          as (map simplify (args-of e))]
      (case op
        :not
        (let [x (first as)]
          (cond
            (boolean? x) (not x)
            (and (op-node? x) (= (op-of x) :not))
            (simplify (first (args-of x)))
            :else (NOT x)))

        :and
        (let [xs (flatten-op :and as)
              xs (distinct xs)
              xs (remove true? xs)]
          (if (some false? xs)
            false
            (case (count xs)
              0 true
              1 (first xs)
              (apply AND xs))))

        :or
        (let [xs (flatten-op :or as)
              xs (distinct xs)
              xs (remove false? xs)]
          (if (some true? xs)
            true
            (case (count xs)
              0 false
              1 (first xs)
              (apply OR xs))))

        :imp
        (simplify (OR (NOT (first as)) (second as)))

        ;; все остальные операции считаем производными
        (simplify (desugar {:op op :args (vec as)}))))

    :else
    (throw (ex-info "Bad expr" {:expr e}))))

;; NNF (нормальная форма по отрицаниям)

(defn nnf
  "Построить НФ по отрицаниям (NNF), где NOT разрешены только
   непосредственно над переменными или константами.
   Использует simplify + desugar, затем применяет законы де Моргана."
  [e]
  (let [e (simplify (desugar e))]
    (cond
      (boolean? e) e
      (symbol? e)  e

      (op-node? e)
      (case (op-of e)
        :not (nnf-neg (first (args-of e)))
        :and (apply AND (map nnf (args-of e)))
        :or  (apply OR  (map nnf (args-of e)))
        (throw (ex-info "Unexpected op in NNF" {:op (op-of e)})))

      :else
      (throw (ex-info "Bad expr" {:expr e})))))

(defn nnf-neg
  "Вспомогательная функция для nnf: обработка выражения под верхней NOT. Реализует законы де Моргана"
  [e]
  (let [e (simplify e)]
    (cond
      (boolean? e) (not e)
      (symbol? e)  (NOT e)

      (op-node? e)
      (case (op-of e)
        :not (nnf (first (args-of e)))
        :and (apply OR  (map nnf-neg (args-of e)))
        :or  (apply AND (map nnf-neg (args-of e)))
        (throw (ex-info "Unexpected op in nnf-neg" {:op (op-of e)})))

      :else
      (throw (ex-info "Bad expr" {:expr e})))))

;; ДНФ

(defn literal?
  "Проверка, что e — литерал:
   * переменная (symbol),
   * отрицание переменной (NOT var),
   * булева константа"
  [e]
  (or (symbol? e)
      (and (op-node? e)
           (= (op-of e) :not)
           (symbol? (first (args-of e))))
      (boolean? e)))

(defn clauses
  "Построить список конъюнктов (списков литералов) из выражения в NNF.
   Предполагается, что e уже упрощено и приведено к форме с :and/:or/:not
   Пример: [[A] [B (NOT C)] ...]."
  [e]
  (let [e (simplify e)]
    (cond
      (= e true)  [[]]
      (= e false) []
      (literal? e) [[e]]

      (and (op-node? e) (= (op-of e) :or))
      (into [] (mapcat clauses (args-of e)))

      (and (op-node? e) (= (op-of e) :and))
      (reduce
       (fn [acc sub]
         (for [c1 acc
               c2 (clauses sub)]
           (concat c1 c2)))
       [[]]
       (args-of e))

      :else
      (throw (ex-info "Expected NNF node" {:expr e})))))

(defn lit-key
  "Ключ для сортировки/сравнения литералов:
   положительный: [:pos var]
   отрицательный: [:neg var]"
  [l]
  (if (symbol? l)
    [:pos l]
    [:neg (first (args-of l))]))

(defn simplify-conj
  "Упростить один конъюнкт (список литералов):
   * убрать дубликаты,
   * проверить противоречие A & !A (тогда вернуть nil),
   * отсортировать литералы по имени и знаку."
  [conj]
  (let [ks  (map lit-key conj)
        pos (set (map second (filter #(= :pos (first %)) ks)))
        neg (set (map second (filter #(= :neg (first %)) ks)))]
    (when (empty? (set/intersection pos neg))
      (->> conj
           distinct
           (sort-by (fn [l]
                      (let [[sgn v] (lit-key l)]
                        [v (if (= sgn :pos) 0 1)])))))))

(defn build-dnf
  "Собрать AST ДНФ из списка конъюнктов (списков литералов).
   Учитывает:
   * пустой список → false,
   * наличие пустой конъюнкции → true,
   * единственный конъюнкт → просто AND от литералов."
  [cls]
  (let [cls (keep simplify-conj cls)
        cls (distinct cls)]
    (cond
      (empty? cls) false
      (some empty? cls) true
      (= 1 (count cls)) (apply AND (first cls))
      :else (apply OR (map #(apply AND %) cls)))))

(defn to-dnf
  "Привести выражение expr к ДНФ.
   Последовательность шагов:
   1) desugar  — понижение синтаксиса,
   2) simplify — локальные упрощения,
   3) nnf      — вынос отрицаний,
   4) clauses  — разбор на конъюнкты,
   5) build-dnf,
   6) финальное simplify.

   Результат логически эквивалентен expr и находится в ДНФ."
  [expr]
  (-> expr
      desugar
      simplify
      nnf
      clauses
      build-dnf
      simplify))

(defn substitute->dnf
  "Сделать подстановку env в expr и привести результат к ДНФ"
  [expr env]
  (to-dnf (subst expr env)))

(defn dnf?
  "Проверка, что выражение e находится в ДНФ.
   По определению:
   * булево/литерал — ДНФ,
   * AND — конъюнкция только из литералов,
   * OR — дизъюнкция литералов и/или конъюнкций литералов"
  [e]
  (let [e (simplify e)]
    (cond
      (boolean? e) true
      (literal? e) true

      (and (op-node? e) (= (op-of e) :and))
      (every? literal? (args-of e))

      (and (op-node? e) (= (op-of e) :or))
      (every? (fn [t]
                (or (literal? t)
                    (and (op-node? t)
                         (= (op-of t) :and)
                         (every? literal? (args-of t)))))
              (args-of e))

      :else false)))

(defn vars-of
  "Вернуть множество переменных (символов), встречающихся в выражении e."
  [e]
  (cond
    (boolean? e) #{}
    (symbol? e)  #{e}

    (op-node? e)
    (apply set/union (map vars-of (args-of e)))

    :else #{}))

(defn eval-expr
  "Вычислить значение выражения expr в окружении env.
   Сначала expr понижается (desugar), затем рекурсивно вычисляется.
   Переменные, отсутствующие в env, трактуются как false"
  [expr env]
  (let [e (desugar expr)]
    (cond
      (boolean? e) e
      (symbol? e)  (boolean (env e))

      (op-node? e)
      (case (op-of e)
        :not (not (eval-expr (first (args-of e)) env))
        :and (every? true? (map #(eval-expr % env) (args-of e)))
        :or  (boolean (some true? (map #(eval-expr % env) (args-of e))))
        (throw (ex-info "Unexpected op in eval" {:op (op-of e)})))

      :else
      (throw (ex-info "Bad expr in eval" {:expr e})))))

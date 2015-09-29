(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc]
                 (if (empty? (rest acc))
                   (first acc)
                   (recur (rest acc))))]
    (helper a-seq)
    ))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1-acc seq2-acc]
                 (cond
                   (and (empty? seq1-acc) (empty? seq2-acc)) true
                   (or (empty? seq1-acc) (empty? seq2-acc)) false
                   :else (let [a (first seq1-acc)
                               b (first seq2-acc)]
                           (if (not= a b)
                             false
                             (recur (rest seq1-acc) (rest seq2-acc))))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         l-seq a-seq]
    (cond
      (empty? l-seq) nil
      (pred (first l-seq)) index
      :else (recur (inc index) (rest l-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         l-seq a-seq]
    (cond
      (empty? l-seq) (if (zero? count) 0 (/ sum count))
      :else (recur (+ sum (first l-seq)) (inc count) (rest l-seq)))))

(defn toggle [a-set a]
  (if (contains? a-set a)
    (disj a-set a)
    (conj a-set a)))

(defn parity [a-seq]
  (loop [acc #{}
         l-seq a-seq]
    (if (empty? l-seq)
      acc
      (recur (toggle acc (first l-seq)) (rest l-seq)))))

(defn fast-fibo [n]
  (if (< n 1)
    0
    (loop [l-n 1
           fn-0 1
           fn-1 0]
      (if (== l-n n)
        fn-0
        (recur (inc l-n) (+ fn-0 fn-1) fn-0)))))

(defn cut-at-repetition [a-seq]
  (loop [reps #{}
         cut []
         l-seq a-seq]
    (let [first (first l-seq)]
      (cond
        (empty? l-seq) cut
        (contains? reps first) cut
        :else (recur (conj reps first) (conj cut first) (rest l-seq))))
    ))


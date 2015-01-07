(ns looping-is-recursion)

(defn power [base exp]
  (loop [res 1, n exp]
    (if (= n 0)
      res
      (recur (* res base) (dec n)))))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [[x & xs] a-seq]
      (if (empty? xs)
        x
        (recur xs)))))

(defn seq= [seq1 seq2]
  (or (and (empty? seq1) (empty? seq2))
      (and (not (empty? seq1)) (not (empty? seq2))
           (= (first seq1) (first seq2))
           (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [i 0, xs a-seq]
    (if (empty? xs)
      nil
      (let [x (first xs)]
        (if (pred x)
          i
          (recur (inc i) (rest xs)))))))

(defn avg [a-seq]
  (loop [sum 0, n 0, xs a-seq]
    (if (empty? xs)
      (/ sum n)
      (recur (+ sum (first xs)) (inc n) (rest xs)))))

(defn parity [a-seq]
  (loop [xs a-seq, res #{}]
    (if (empty? xs)
      res
      (let [x (first xs)]
        (recur (rest xs) (if (contains? res x)
                           (disj res x)
                           (conj res x)))))))

(defn fast-fibo [n]
  (if (< n 2)
    n
    (loop [i 2, i-2 0, i-1 1]
      (if (= i n)
        (+ i-2 i-1)
        (recur (inc i) i-1 (+ i-2 i-1))))))

(defn cut-at-repetition [a-seq]
  (loop [xs a-seq, uniq #{}, res (empty a-seq)]
    (if (empty? xs)
      res
      (let [x (first xs)]
        (if (contains? uniq x)
          res
          (recur (rest xs) (conj uniq x) (conj res x)))))))


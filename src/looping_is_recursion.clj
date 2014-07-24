(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp)
    1
    (let [helper (fn [base acc exp]
                   (if (= exp 0)
                     acc
                     (recur base (* acc base) (dec exp))))]
      (helper base 1 exp))))

(defn last-element [a-seq]
  (let [helper (fn [seq]
                 (if (empty? (rest seq))
                   (first seq)
                   (recur (rest seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                   (and (empty? a-seq) (empty? b-seq))
                     true
                   (or (empty? a-seq) (empty? b-seq))
                     false
                   (= (first a-seq) (first b-seq))
                     (recur (rest a-seq) (rest b-seq))
                   :else
                     false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [f pred
         seq a-seq
         i 0]
    (cond
     (empty? seq)
     nil
     (f (first seq))
      i
     :else
     (recur f (rest seq) (inc i)))))

(defn avg [a-seq]
  (loop [av 0
         acc 1
         seq a-seq]
    (cond
     (empty? seq)
       (/ av acc)
     :else
       (recur (+ av (first a-seq)) (inc acc) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [ret #{}
         seq a-seq]
    (if (empty? a-seq)
      ret
      (recur (toggle ret (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (cond
   (<= n 1) n
   :else
     (loop [prev 0
         xprev 1
         i 2
         aim n]
       (if (= i aim)
         (+ prev xprev)
         (recur prev (+ prev xprev) (inc i) aim)))))

(defn cut-at-repetition [a-seq]
  (loop [ret []
         seq a-seq]
    (if (or (contains? ret (first a-seq)) (empty? a-seq))
      ret
      (recur (conj ret (first a-seq)) (rest a-seq)))))

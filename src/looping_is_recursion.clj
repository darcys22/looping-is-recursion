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
  (loop [n 0,
         total 0,
         seq a-seq]
    (if (empty? seq)
      (/ total n)
      (recur (inc n) (+ total (first seq)) (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [ret #{},
         sea a-seq]
    (if (empty? sea)
      ret
      (recur (toggle ret (first sea)) (rest sea)))))

(defn fast-fibo [n]
  (cond
   (<= n 1) n
   :else
     (loop [prev 0
         xprev 1
         i 1
         aim n]
       (if (= i aim)
         (+ prev xprev)
         (recur (+ prev xprev) prev (inc i) aim)))))

(defn cut-at-repetition [a-seq]
  (loop [ret #{}
         uniq []
         sea a-seq]
    (if (or (contains? ret (first sea)) (empty? sea))
      uniq
      (recur (conj ret (first sea)) (conj uniq (first sea)) (rest sea)))))

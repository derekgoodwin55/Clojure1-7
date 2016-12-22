(defn replace-with-sum [v]
  (loop [i 0 sum 0 newv [] hasnum false]
    (if (== i (count v)) (if hasnum (conj newv sum) newv)
      (if (number? (get v i)) (recur (inc i) (+ sum (get v i)) newv true)
        (recur (inc i) sum (conj newv (get v i)) hasnum)
      )
    )
   )
)


(defn running-sum [v]
(loop [i 0 sum 0 newv []]
  (if (== i (count v)) newv
      (recur (inc i) (+ sum (get v i)) (conj newv (+ (get v i) sum))
))))


(defn expand [l]
(loop [ i 0 newl '() ]
    (if (== i (count l)) (reverse newl)
      (if (coll? (nth l i))
        (recur (inc i) (conj newl (repeat (first (nth l i)) (second (nth l i)))))
        (recur (inc i) (conj newl (nth l i)))
     )
    )
)
)


(defn factors [r i]
  (for [x r :let [y (mod i x)] :when (== y 0)] x)
)


(defn string->ascii [s]
(let [charr (vec (char-array s))]
(loop [i 0 rval []]
  (if (== i (count charr)) (seq rval)
    (recur (inc i) (conj rval (- (int (get charr i)) 32)))
))))


(defn ascii-num96 [l]
(let [reversed (rseq (vec l))]
(loop [i 0 base 1 rsum 0]
  (if (== i (count reversed)) rsum
    (recur (inc i) (* base 96) (+ rsum (* (nth reversed i) base))
)))))


(defn make-dict [v]
(loop [i 0 rval {}]
  (if (== i (count v)) rval
  (recur (inc i) (assoc rval (get v i) (ascii-num96 (string->ascii (get v i)))))
)))


(defn in-dict [m word]
(if (contains? m word) (get m word) nil
))

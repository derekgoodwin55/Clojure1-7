(def dc 5)
(def cc 3)
(def dd 1)
(def cd 0)

(defn all-defect [ownHistory oppHistory]
'd)

(defn all-cooperate [ownHistory oppHistory]
'c)

(defn random [ownHistory oppHistory]
  (let [x (rand-int 2)]
    (if (= x 0) 'c 'd))
)

(defn tit-for-tat [ownHistory oppHistory]
  (if (empty? oppHistory) 'c
      (first oppHistory) 
))

(defn tit-for-two-tats [ownHistory oppHistory]
  (if (< (count oppHistory) 2) 'c
    (if (and (= (first oppHistory) 'd) (= (second oppHistory) 'd)) 'd 'c
)))

(defn compete-n-times [s1 s2 n]
  (loop [game 0 h1 '() h2 '() score1 0 score2 0]
  (do (def val1 (s1 h1 h2)) (def val2 (s2 h2 h1))

    (cond (= game n) (vector score1 score2)

      (and (= val1 'c) (= val2 'c))
        (recur (inc game) (conj h1 val1) (conj h2 val2) (+ score1 cc) (+ score2 cc))

      (and (= val1 'c) (= val2 'd))
        (recur (inc game) (conj h1 val1) (conj h2 val2) (+ score1 cd) (+ score2 dc))

      (and (= val1 'd) (= val2 'c))
        (recur (inc game) (conj h1 val1) (conj h2 val2) (+ score1 dc) (+ score2 cd))

      :else
        (recur (inc game) (conj h1 val1) (conj h2 val2) (+ score1 dd) (+ score2 dd))
))))


;; Same as above but with Atoms. Not used do to full functionality except persistent list error

"(defn compete-n-times [s1 s2 n]
  (loop [game 0 h1 (atom ()) h2 (atom ()) val1 (s1 h1 h2) val2 (s2 h2 h1) score1 0 score2 0]
    (cond (= game n) (vector score1 score2)
      (and (= val1 'c) (= val2 'c))
        (recur (inc game) (swap! h1 conj (s1 @h1 @h2)) (swap! h2 conj (s2 @h2 @h1)) (s1 h1 h2) (s2 h2 h1) (+ score1 cc) (+ score2 cc))
      (and (= val1 'c) (= val2 'd))
        (recur (inc game) (swap! h1 conj (s1 @h1 @h2)) (swap! h2 conj (s2 @h2 @h1)) (s1 h1 h2) (s2 h2 h1) (+ score1 cd) (+ score2 dc))
      (and (= val1 'd) (= val2 'c))
        (recur (inc game) (swap! h1 conj (s1 @h1 @h2)) (swap! h2 conj (s2 @h2 @h1)) (s1 h1 h2) (s2 h2 h1) (+ score1 dc) (+ score2 cd))
      :else
        (recur (inc game) (swap! h1 conj (s1 @h1 @h2)) (swap! h2 conj (s2 @h2 @h1)) (s1 h1 h2) (s2 h2 h1) (+ score1 dd) (+ score2 dd))
)))"

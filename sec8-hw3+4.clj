;; Helper Functions

(defn egcd "return list (s g t) s.t. a s + b t = g = gcd(a,b)" [a b]
  (letfn [(sign [x] (cond (pos? x) 1 (neg? x) -1 (zero? x) 0 :else nil))
	  (abs [x] (if (neg? x) (- x) x))]
	 ;; extended GCD
	 (cond (or (< a 0) (< b 0)) (map * (list (sign a) 1 (sign b)) (egcd (abs a) (abs b)))
	       (< a b) (reverse (egcd b a))
	       (== b 0) (list 1 a 0)
	       (== b 1) (list 0 1 1)
	       :else (let [q (quot a b)
			     r (mod a b)
			     w (egcd b r)]
			  (list (last w)
				(second w)
				(- (first w) (* q (last w))))))))

(defn modular-inverse "compute b s.t. a*b = 1 mod n, if it exists." [a n]
  (if (== n 0)
      (/ a)
      (let [result (egcd n a)]
           (if (== (second result) 1) ;; check if inverse exists...
               (mod (last result) n)
               nil))))

(defn gcd [a b]
  (if (= b 0) a
    (recur b (mod a b))
  )
)

(defn rand-bigint "crude random number of type bigint" [n] 
  (bigint (bigdec (rand n))))


(defn string->ascii [s]
  (let [charr (vec (char-array s))]
    (loop [i 0 rval []]
      (if (== i (count charr)) (seq rval)
        (recur (inc i) (conj rval (- (int (get charr i)) 32)))))))


(defn ascii-num91 [l]
(let [reversed (rseq (vec l))]
  (loop [i 0 base 1 rsum 0]
    (if (== i (count reversed)) rsum
      (recur (inc i) (* base 91) (+ rsum (* (nth reversed i) base)))))))

(defn expmod "compute base^exp mod m, fast" [base exp m]
  (cond (== exp 0)   1
	(neg? exp)  'error
        (even? exp) (mod ((fn [x] (* x x)) (expmod base (/ exp 2) m))
			 m)
	:else       (mod (* base (expmod base (- exp 1) m))
			 m)))

(defn from-dec [n base]
(loop [n n base base newl '()]
    (if (== n 0) newl
      (recur (quot n base) base (conj newl (mod n base)))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def p 6240322667N)
(def q 6240323147N)
(def n (* p q))
(def m (* (- p 1) (- q 1)))

(defn find-e [m]
  (loop [e (rand-bigint m)]
    (if (= 1 (gcd e m))
      e
      (recur (rand-bigint m)))))

(def e (find-e m))
(def d (modular-inverse e m))


(defn make-public-key [n e]
  (list n e))

(def public-key (make-public-key n e))
(def private-key (make-public-key d e))

(defn public-mod [publickey]
  (first publickey))

(defn public-exp [publickey]
  (last publickey))


(defn encrypt-num [number publickey]
  (expmod number e n))

(defn decrypt-num [number privatekey]
  (expmod number d n))

(defn encrypt-word [plaintext publickey]
    (let [charr (vec (char-array plaintext))]
    (loop [i 0 rval []]
      (if (== i (count charr)) (encrypt-num (ascii-num91 (seq rval)) public-key)
        (recur (inc i) (conj rval (- (int (get charr i)) 32)))))))

(encrypt-word "goodw171" public-key)

(defn decrypt-word [cyphertext privatekey]
  (let [ intarr (from-dec (expmod cyphertext d n) 91)]
    (loop [i 0 newl '()]
      (if (== i (count intarr)) (clojure.string/join (reverse newl))
        (recur (inc i) (conj newl (char (+ (nth intarr i) 32))))))))

(defn encrypt-msg [msg publickey]
(for [x (partition-by (fn[x] (= x \space)) msg) :let [y (encrypt-word x public-key)]] y))

(defn decrypt-msg [msg privatekey]
(clojure.string/join (for [x msg :let [y (if (= x 0) \space (decrypt-word x private-key))]] y)))

(defn encrypt-val [w key]
  (encrypt-word w public-key))

(defn decrypt-val [c key]
  (decrypt-word c private-key))

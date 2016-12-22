;; Derek Goodwin
;; 3862569
;; goodw171@umn.edu

(defn interl+ [lst1 lst2]
(loop [i 0 rVal []]
  (if (and (>= i (count lst1)) (>= i (count lst2))) (seq rVal)
    (recur (inc i) (conj rVal (nth lst1 (mod i (count lst1))) (nth lst2 (mod i (count lst2))))))))

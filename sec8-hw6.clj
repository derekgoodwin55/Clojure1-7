;; Derek Goodwin 3862569 goodw171@umn.edu;;

(use 'clojure.java.io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMPORTANT NOTE: space added at end of re-find string for correctness ;;

(defn count-IP []
  (let [count (atom 0)]
  (with-open [rdr (reader "NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (not= nil (re-find #"^\d{1,3}[.]\d{1,3}[.]\d{1,3}[.]\d{1,3} " line)) (swap! count inc)
	)))@count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMPORTANT NOTE: **Bottom version appends elements to an atom list**, top version counts total elements ;;

(defn find-private-IP []
  (let [count (atom 0)]
  (with-open [rdr (reader "/home/goodw171/Desktop/csci2041-s16/NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
	(if (or (re-find #"^192[.]0[.]0[.]\d{1,3}" line) (re-find #"^10[.]\d{1,3}[.]\d{1,3}[.]\d{1,3}" line) (re-find #"^192[.]168[.]\d{1,3}[.]\d{1,3}" line) (re-find #"^192[.]88[.]99[.]\d{1,3}" line)) (swap! count inc)
  )))@count))

(defn find-private-IP []
  (let [rList (atom '())]
  (with-open [rdr (reader "/home/goodw171/Desktop/csci2041-s16/NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
	(if (or (re-find #"^192[.]0[.]0[.]\d{1,3}" line) (re-find #"^10[.]\d{1,3}[.]\d{1,3}[.]\d{1,3}" line) (re-find #"^192[.]168[.]\d{1,3}[.]\d{1,3}" line) (re-find #"^192[.]88[.]99[.]\d{1,3}" line)) (swap! rList conj (re-find #"^\d{1,3}[.]\d{1,3}[.]\d{1,3}[.]\d{1,3}" line)))
))@rList))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMPORTANT NOTE: **Bottom version appends elements to an atom list**, Top version counts total elements ;;

(defn count-request-on-dates []
  (let [count (atom 0)]
  (with-open [rdr (reader "NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (or (= (re-find #"\d\d[/]\w\w\w[/]\d\d\d\d" line) "04/Jul/1995") (= (re-find #"\d\d[/]\w\w\w[/]\d\d\d\d" line) "05/Jul/1995")) (swap! count inc)
)))@count))

(defn count-request-on-dates []
  (let [rList (atom '())]
  (with-open [rdr (reader "NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (or (= (re-find #"\d\d[/]\w\w\w[/]\d\d\d\d" line) "04/Jul/1995") (= (re-find #"\d\d[/]\w\w\w[/]\d\d\d\d" line) "05/Jul/1995")) (swap! rList conj (re-find #"\d\d[/]\w\w\w[/]\d\d\d\d" line))
)))@rList))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;IMPORTANT NOTE: I was unsure of what distinct hosts were, so I added the condition the host had to be an IP address 

(defn count-hosts []
  (let [count (atom 0)]
  (with-open [rdr (reader "/home/goodw171/Desktop/csci2041-s16/NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (and (or (re-find #"[0][5][/]\w\w\w[/]\d\d\d\d[:][2][2][:][0]\d[:]\d\d" line) (re-find #"[0][5][/]\w\w\w[/]\d\d\d\d[:][2][2][:][1]\d[:]\d\d[ ]" line) (re-find #"[0][5][/]\w\w\w[/]\d\d\d\d[:][2][2][:][2]\d[:]\d\d[ ]" line)) (re-find #"^\d" line)) (swap! count inc)
)))@count))

(defn count-hosts []
  (let [rList (atom '())]
  (with-open [rdr (reader "/home/goodw171/Desktop/csci2041-s16/NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (and (or (re-find #"[0][5][/]\w\w\w[/]\d\d\d\d[:][2][2][:][0]\d[:]\d\d" line) (re-find #"[0][5][/]\w\w\w[/]\d\d\d\d[:][2][2][:][1]\d[:]\d\d[ ]" line) (re-find #"[0][5][/]\w\w\w[/]\d\d\d\d[:][2][2][:][2]\d[:]\d\d[ ]" line)) (re-find #"^\d" line)) (swap! rList conj line)
)))@rList))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMPORTANT NOTE: **Bottom version appends elements to an atom list**, top version counts total elements ;;


(defn find-server-error []
  (let [count (atom 0)]
  (with-open [rdr (reader "/home/goodw171/Desktop/csci2041-s16/NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (re-find #"[ ][5]\d\d[ ]" line) (swap! count inc)
)))@count))

(defn find-server-error []
  (let [rList (atom '())]
  (with-open [rdr (reader "/home/goodw171/Desktop/csci2041-s16/NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (re-find #"[ ][5]\d\d[ ]" line) (swap! rList conj (re-find #"[ ][5]\d\d[ ]" line))
)))@rList))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-redirect []
  (let [count (atom 0)]
  (with-open [rdr (reader "/home/goodw171/Desktop/csci2041-s16/NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (re-find #"[ ][3]\d\d[ ]" line) (swap! count inc)
)))@count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-morethan50000 []
  (let [count (atom 0)]
  (with-open [rdr (reader "/home/goodw171/Desktop/csci2041-s16/NASA_access_log_Jul95_short")]
  (doseq [line (line-seq rdr)]
  (if (and (not= (re-find #"\d*$" line) "") (>= (Integer/parseInt (re-find #"\d*$" line)) 50000)) (swap! count inc) nil)))@count))

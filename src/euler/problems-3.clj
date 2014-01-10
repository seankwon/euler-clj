(ns euler.problems-3)

;;problem 18
(def data [[75]
           [95 64]
           [17 47 82]
           [18 35 87 10]
           [20  4 82 47 65]
           [19  1 23 75  3 34]
           [88  2 77 73  7 63 67]
           [99 65  4 28  6 16 70 92]
           [41 41 26 56 83 40 80 70 33]
           [41 48 72 33 47 32 37 16 94 29]
           [53 71 44 65 25 43 91 52 97 51 14]
           [70 11 33 28 77 73 17 78 39 68 17 57]
           [91 71 52 38 17 14 91 43 58 50 27 29 48]
           [63 66 4 68 89 53 67 30 73 16 69 87 40 31]
           [4 62 98 27 23  9 70 98 73 93 38 53 60 4 23]])

(defn merge-rows [a b]
  (map + (map #(apply max %) (partition 2 1 a)) b))

(reduce merge-rows (reverse data))

;;problem 19
;;Jan 1st 1901 -> tuesday

(defn days-in-months [leap]
  (if (true? leap)
    [31 29 31 30 31 30 31 31 30 31 30 31]
    [31 28 31 30 31 30 31 31 30 31 30 31]))

(def century
  (vec (flatten (map days-in-months (map #(zero? (mod % 4)) (range 1901 2001))))))

(defn find-first [coll index]
  (mod (+ (nth coll index) (nth coll (inc index))) 7))

(defn find-suns [coll index]
  (lazy-seq
   (when (not= index (dec (count coll)))
     (let [first-m (find-first coll index)]
       (cons first-m (find-suns (assoc coll (inc index) first-m) (inc index)))))))

(def count-sundays (count (filter #(= 0 %) (find-suns century 0))))

count-sundays

;;problem 20

(defn factorial [n]
  (reduce *' (range 1 (inc n))))

(def sum-digits
  (reduce + (map #(. Integer parseInt %)
                 (rest (list* (clojure.string/split (str (factorial 100)) #""))))))

;;problem 21

(defn find-sum-divisors [n]
  (reduce + (filter #(zero? (rem n %)) (range 1 (inc (int (/ n 2)))))))

(defn is-amicable? [n]
  (and (not= (find-sum-divisors n) n) (= n (find-sum-divisors (find-sum-divisors n)))))

(defn find-amicable-pairs [end]
  (filter is-amicable? (range 1 (inc end))))

(def sum-amicable-pairs-to-10000 (reduce + (find-amicable-pairs 10000)))

;;problem 22
(def letter-map (zipmap (map str (map char (range 65 91))) (range 1 27)))

(def vec-names
  (sort (conj
   (pop (clojure.string/split (str (slurp "src/euler/names.txt")) #","))
   "ALONSO")))

(defn get-letters [s]
  (rest (list* (clojure.string/split s #""))))

(defn find-score [s]
  (reduce + (map #(letter-map %) (get-letters s))))

(def accumulate-scores
  (let [vec->nestedvec (fn [a b] (vector a b))
        rdc-score-indx (fn [a] (* (nth a 0) (nth a 1)))
        names-scores (map find-score vec-names)
        scores-size (inc (count names-scores))]
    
    (reduce + (map rdc-score-indx
         (map vec->nestedvec names-scores (range 1 scores-size))))))

(ns euler.problems-1)

;; problem 2


(def even-fibs-sum-seq-four-mil
  (let [fib       (fn [x y]
                    (lazy-seq (cons y (fib y (+ x y)))))
        even-fibs (filter even? (fib 1 1))]
    (reduce + (take-while #(< % 4000000) even-fibs))))

;;problem 3, learn and reread

(def largest-prime [n]
  (let [is-prime? (fn [x]
                    (zero? (count (filter
                                   #(zero? (rem n %)) (range 3 n 2)))))
        big-number (Math/sqrt n)]
    (first (filter #(is-prime? %)
                   (filter #(zero? (rem n %))
                           (reverse (range 3 big-number 2)))))))

;;problem 4 ultra messy, but eh it works.
 
(def palindromic-product 
  (let [pos-threes (range 100 999)
        is-six-palindrome (fn [x]
                            (= (str (int (double (/ x 1000))))
                               (clojure.string/join 
                                 (reverse 
                                    (str (- x (* (int (/ x 1000)) 1000)))))))
        palindromes (filter is-six-palindrome? (reverse (range 100000 999999)))]
    (loop [palindrome palindromes pos-threes pos-threes]
      (if (and
           (and (not (zero? (count palindrome))) (not (zero? (count pos-threes))))
           (zero? (rem (first palindrome) (first pos-threes))) 
           (<= (count (str (/ (first palindrome) (first pos-threes)))) 3))
        (first palindrome)
        (recur (if (zero? (count pos-threes))
                 (rest palindrome)
                 palindrome)
               (if (zero? (count pos-threes))
                 (reverse (range 100 999))
                 (rest pos-threes)))))))

;;problem 5

(def small-mult
  (let [div-10 2520
        inc-div-20? (fn [x]
                      (= (count (filter #(zero? (rem n %)) (range 11 21))) 10))]
    (loop [n div-20]
      (if (true (inc-div-20? n))
        x
        (recur (+ div-10 n))))))

;;problem 6

(def square-sum-below-100
  (let [sq-sum (reduce + (range 1 101))
        sum-sq (reduce + (map #(* % %) (range 1 101)))]
    (- (* sq-sum sq-sum) sum-sq)))

;;problem 7 read this one

(defn sieve [potentials primes]
  (if-let [p (first potentials)]
    (recur (doall (filter #(not= (mod % p) 0) potentials)) (conj primes p))
    primes))
(nth (sieve (range 2 120000) []) 10000)

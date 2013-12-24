(ns euler.problems-1)

;; problem 2
(def even-fibs-sum-seq-four-mil
  (let [fib       (fn [x y]
                    (lazy-seq (cons y (fib y (+ x y)))))
        even-fibs (filter even? (fib 1 1))]
    (reduce + (take-while #(< % 4000000) even-fibs))))

(ns com.benjaminbinford.day1
  (:require [clojure.string :as str])
  (:gen-class))

(def input (slurp "resources/input.txt"))
(def sample (slurp "resources/sample.txt"))

(defn- columnn-reducer [[c1 c2] r]
  ; split the row into two numeric values
            ; at whitespace, and push the first value
            ; onto c1 and the second value onto c2
  (let [[a b] (map read-string (clojure.string/split r #"\s+"))]
    [(conj c1 a) (conj c2 b)]))



(defn columns [input]
  (map sort
       (reduce columnn-reducer
               [[] []]
               (clojure.string/split input #"\n"))))

(columnn-reducer [[] []] "1 2")
(columnn-reducer [[4] [3]] "1 2")


(columns sample)

(defn simple-diff [[c1 c2]]
  (abs (- (first c1) (first c2))))

(simple-diff (columns sample))

(defn simple-diff-sum
  ([cs] (simple-diff-sum cs 0))

  ([[c1 c2] acc]
   (if (empty? c1)
     acc
     (recur [(rest c1) (rest c2)] (+ acc (simple-diff [c1 c2]))))))


(defn solve1 [input]
  (simple-diff-sum (columns input)))

(solve1 sample)
(solve1 input)


(defn histogram
  "given a vector of numbers, return a map of how many times each number appears in the list"
  [cs]
  (reduce
   (fn [m n]
     (assoc m n (inc (get m n 0))))
   {}
   cs))


(histogram (second (columns sample)))

(defn calc-similarity [n m]
  (* n (get m n 0)))

(calc-similarity 3 (histogram (second (columns sample))))

(defn calc-similarity-sum
  [input]
  (let [[c1 c2] (columns input)
        m (histogram c2)]
    (reduce
     (fn [acc n]
       (+ acc (calc-similarity n m)))
     0
     c1)))

(calc-similarity-sum sample)
;;=> 31
(calc-similarity-sum input)
;;=> 19678534
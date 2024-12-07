(ns com.benjaminbinford.day7
  (:require [clojure.string :as str])
  (:gen-class))




(def input (str/split-lines (slurp "resources/input.txt")))
(def sample (str/split-lines (slurp "resources/sample.txt")))

(def ops [* +])

(defn parse-line [l]
  (let [[lhs rhs] (str/split l #":")
        nums (read-string (str "[" rhs "]"))
        lhs (read-string lhs)]
    [lhs nums]))

(parse-line "10: 1 2 3")
  ;;=> [10 [1 2 3]]

(defn calc-line
  ([test-value nums total]
   (if (empty? nums)
     (if (= test-value total)
       true
       false)
     (or (calc-line test-value (rest nums) (* total (first nums)))
         (calc-line test-value  (rest nums) (+ total (first nums))))))
  ([[test-value nums]]
   (calc-line test-value (rest nums) (first nums))))


(calc-line [190 [10 19]])
;;=> true

(defn part1 [input]
  (reduce + (map first (filter #(calc-line %) (map parse-line input)))))
;;=> #'com.benjaminbinford.day7/part1

(part1 sample)
;;=> 3749
(part1 input)
;;=> 7885693428401

(defn concat-nums [a b]
  (read-string (str a b)))

(concat-nums 48 6)
;;=> 486


(defn calc-line2
  ([test-value nums total]
   (if (empty? nums)
     (if (= test-value total)
       true
       false)
     (or (calc-line2 test-value (rest nums) (* total (first nums)))
         (calc-line2 test-value  (rest nums) (+ total (first nums)))
         (calc-line2 test-value  (rest nums) (concat-nums total (first nums))))))
  ([[test-value nums]]
   (calc-line2 test-value (rest nums) (first nums))))



(defn part2 [input]
  (reduce + (map first (filter #(calc-line2 %) (map parse-line input)))))

(part2 sample)
;;=> 11387


(part2 input)
;;=> 348360680516005
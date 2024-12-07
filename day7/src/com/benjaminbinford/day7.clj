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
  ([ops test-value nums total]
   (if (empty? nums)
     (if (= test-value total)
       true
       false)
     (some identity
           (map #(calc-line ops test-value (rest nums) (% total (first nums)))
                ops))))
  ([[test-value nums] ops]
   (calc-line ops test-value (rest nums) (first nums))))




(calc-line [190 [10 19]] ops)
;;=> true

(defn calculate [input ops]
  (reduce + (map first (filter #(calc-line % ops) (map parse-line input)))))

(defn part1 [input]
  (calculate input ops))
;;=> #'com.benjaminbinford.day7/part1

(part1 sample)
;;=> 3749
(part1 input)
;;=> 7885693428401

(defn concat-nums [a b]
  (read-string (str a b)))

(concat-nums 48 6)
;;=> 486



(def ops2 [* + concat-nums])


(defn part2 [input]
  (calculate input ops2))

(part2 sample)
;;=> 11387


(part2 input)
;;=> 348360680516005
(ns com.benjaminbinford.day2
  (:require [clojure.string :as str])
  (:gen-class))


(def input (slurp "resources/input.txt"))
(def sample (slurp "resources/sample.txt"))

(defn- split-to-numbers [s]
  (map read-string (str/split s #" ")))

(split-to-numbers "1 2 3 4 5")
;;=> (1 2 3 4 5)

(defn- prepare [i]
  (map split-to-numbers (str/split-lines i)))


(prepare sample)
;;=> ((7 6 4 2 1) (1 2 7 8 9) (9 7 6 2 1) (1 3 2 4 5) (8 6 4 4 1) (1 3 6 7 9))

(defn- compute-differences
  "Given a list of numbers, return the difference between each number as another vector"
  [numbers]
  (map (fn [[a b]] (- b a)) (partition 2 1 numbers)))

(compute-differences [1 2 5 4 5])
;;=> (1 3 -1 1)

(defn- same-sign?
  "return true if all numbers in the list have the same sign"
  [numbers]
  (let [sign (> 0 (first numbers))]
    (every? #(= sign (> 0 %)) numbers)))

(same-sign? [1 2 3])
;;=> true

(same-sign? [1 -2 3])
;;=> false

(defn- differences-in-tolerance
  "return true if the abs value of each numbers in the list is at least 1 or at most 3"
  [diffs]
  (every? #(<= 1 (Math/abs %) 3) diffs))

(differences-in-tolerance [1 2 3])
;;=> true
(differences-in-tolerance [1 2 4])
;;=> false

(differences-in-tolerance [1 2 0])
;;=> false

(defn- day1-predicate [nums]
  (let [diffs (compute-differences nums)]
    (and (same-sign? diffs) (differences-in-tolerance diffs))))

(defn- day1 [i]
  (let [nums (prepare i)]
    (count (filter day1-predicate nums))))

(defn- remove-one [nums]
  (map (fn [i] (concat (take i nums) (drop (inc i) nums)))
       (range (count nums))))

(remove-one [1 2 3 4])
;;=> ((2 3 4) (1 3 4) (1 2 4) (1 2 3))

(defn safe-with-one-removal? [nums]
  (or (day1-predicate nums)
      (some day1-predicate
            (remove-one nums))))

(defn- day2 [i]
  (let [nums-list (prepare i)]
    (count (filter safe-with-one-removal? nums-list))))



(day1 sample)
;;=> 2
(day1 input)
;;=> 282

(day2 sample)
;;=> 6

(day2 input)
;;=> 349

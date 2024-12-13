(ns com.benjaminbinford.day13
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-button
  "Match a string like \"Button A: X+94, Y+34\"
   where A can be any character and the return [94 34]"
  [input]
  (let [[_ x y] (re-find #"Button [A-Z]: X[+](\d+), Y[+](\d+)" input)]
    [(Integer/parseInt x) (Integer/parseInt y)]))


(parse-button "Button A: X+94, Y+34")
;;=> [94 34]


(defn parse-prize
  "Match a string like \"Prize: X=94, Y=34\"
   where A can be any character and the return [94 34]"
  [input]
  (let [[_ x y] (re-find #"Prize: X=(\d+), Y=(\d+)" input)]
    [(Integer/parseInt x) (Integer/parseInt y)]))


(defn parse-record [input]
  (let [[buttona buttonb prize] (str/split-lines input)]
    {:a (parse-button buttona)
     :b (parse-button buttonb)
     :prize (parse-prize prize)}))

(defn parse [input]
  (let [records (str/split input #"\r?\n\r?\n")]
    (mapv parse-record records)))



(def input (parse (slurp "resources/input.txt")))
(def sample (parse (slurp "resources/sample.txt")))

sample
;;=> [{:a [94 34], :b [22 67], :prize [8400 5400]}
;;    {:a [26 66], :b [67 21], :prize [12748 12176]}
;;    {:a [17 86], :b [84 37], :prize [7870 6450]}
;;    {:a [69 23], :b [27 71], :prize [18641 10279]}]

(defn find-ways [{:keys [a b prize]}]
  (for [a-presses (range 101)
        b-presses (range 101)
        :when (and
               (= (+ (* a-presses (first a)) (* b-presses (first b))) (first prize))
               (= (+ (* a-presses (second a)) (* b-presses (second b))) (second prize)))]
    [a-presses b-presses]))

(find-ways {:a [94 34], :b [22 67], :prize [8400 5400]})
;;=> ([80 40])

(find-ways {:a [26 66], :b [67 21], :prize [12748 12176]})
;;=> ()

(find-ways {:a [17 86], :b [84 37], :prize [7870 6450]})
;;=> ([38 86])

(find-ways {:a [69 23], :b [27 71], :prize [18641 10279]})
;;=> ()

(defn find-cost [record]
  (let [ways (find-ways record)]
    (if (empty? ways)
      0
      (apply min (map #(+ (* 3 (first %)) (second %)) ways)))))

(find-cost {:a [94 34], :b [22 67], :prize [8400 5400]})
;;=> 280

(defn find-costs [records]
  (reduce + (map find-cost records)))

(find-costs sample)
;;=> 480

(find-costs input)
;;=> 29201

; Button A: X+94, Y+34
; Button B: X+22, Y+67
; Prize: X=8400, Y=5400
; a*m + b *n = x
; a*o + b*p = y
; m = 94, n = 22, x = 8400
; o = 34, p = 67, y = 5400
; a = (yn - px) / (on - mp) where on - mp != 0
; b = (ym - ox) / (pm - no) where pm - no != 0
(defn solveA [x y m n o p]
  (let [denominator (- (* o n) (* m p))]
    (if (zero? denominator)
      nil
      (let [a (/ (- (* y n) (* p x)) denominator)]
        (if (ratio? a)
          nil
          a)))))

(solveA 8400 5400 94 22 34 67)
;;=> 80

(defn solveB [x y m n o p]
  (let [denominator (- (* p m) (* n o))]
    (if (zero? denominator)
      nil
      (let [b (/ (- (* y m) (* o x)) denominator)]
        (if  (ratio? b)
          nil
          b)))))

(solveB 8400 5400 94 22 34 67)
;;=> 40


(solveA 10000000008400 10000000005400 94 22 34 67)
;;=> nil
(solveB 10000000008400 10000000005400 94 22 34 67)
;;=> nil

(solveA 10000000012748 10000000012176 26 67 66 21)
;;=> 118679050709

(solveB 10000000012748 10000000012176 26 67 66 21)
;;=> 103199174542




(defn find-cost2 [{:keys [a b prize]}]
  (println a b prize)
  (let [x (first prize)
        y (second prize)
        m (first a)
        n (first b)
        o (second a)
        p (second b)

        a (solveA x y m n o p)
        b (solveB x y m n o p)]
    (if (or (nil? a) (nil? b))
      0
      (+ (* 3 a) b))))


(defn find-costs2 [records]
  (reduce + (map find-cost2 records)))



(find-cost2 {:a [94 34], :b [22 67], :prize [8400 5400]})


(find-costs2 sample)
;;=> 480

(find-costs2 input)
;;=> 29201

(defn update-prize [[x y]]
  [(+ x 10000000000000) (+ y 10000000000000)])

(find-costs2 (map #(update % :prize update-prize) input))
;;=> 104140871044942
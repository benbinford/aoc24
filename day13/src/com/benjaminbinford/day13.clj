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
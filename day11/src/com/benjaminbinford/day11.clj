(ns com.benjaminbinford.day11
  (:gen-class))

(def input [0 27 5409930 828979 4471 3 68524 170])
(def sample [125 17])

(defn parse-int [s]
  (Integer/parseInt s))

(defn step-stone [stone]
  (let [rep (str stone)]
    (cond
      (= stone 0)
      [1]

      (= 0 (bit-and 1 (count rep)))
      [(parse-int
        (subs rep 0 (/ (count rep) 2)))
       (parse-int
        (subs rep (/ (count rep) 2)))]

      :else
      [(* stone 2024)])))

(step-stone 0)
;;=> [1]
(step-stone 3)
;;=> [6072]
(step-stone 1000)
;;=> [10 0]



(def step
  (memoize (fn [input count]
             (if (zero? count)
               input
               (vec (mapcat #(step (step-stone %) (dec count)) input))))))



(step-stone 512072)
;;=> [512 72]
(step sample 1)
;;=> [253000 1 7]
(step sample 2)
;;=> [253 0 2024 14168]
(step sample 3)
;;=> [512072 1 20 24 28676032]
(step sample 4)
;;=> [512 72 2024 2 0 2 4 2867 6032]
(step sample 5)
;;=> [1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32]

(step sample 6)
;;=> [2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2]

(count (step sample 25))
;;=> 55312

(count (step input 25))
;;=> 


(count (step input 45))

(count (step [0] 35))
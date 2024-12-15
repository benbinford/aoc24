(ns com.benjaminbinford.day14
  (:require [clojure.string :as str]
            [clojure.math :refer [floor]])
  (:gen-class))


(defn parse-robot [input]
  (let [[_ i j di dj] (re-find #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" input)]
    {:p [(Integer/parseInt i) (Integer/parseInt j)]
     :v [(Integer/parseInt di) (Integer/parseInt dj)]}))

(parse-robot "p=0,4 v=3,-3")
;;=> {:p [0 4], :v [3 -3]}
(parse-robot "p=7,3 v=-1,2")
;;=> {:p [7 3], :v [-1 2]}


(defn update-positions [robots]
  (assoc robots :positions (set (map :p (:robots robots)))))

(defn parse [input]
  (let [records (str/split-lines input)
        robots (mapv parse-robot (rest records))]
    (update-positions {:bounds (read-string (first records)) :robots robots})))



(def input (parse (slurp "resources/input.txt")))
(def sample (parse (slurp "resources/sample.txt")))

sample
;;=> {:bounds [11 7],
;;    :robots
;;    [{:p [0 4], :v [3 -3]}
;;     {:p [6 3], :v [-1 -3]}
;;     {:p [10 3], :v [-1 2]}
;;     {:p [2 0], :v [2 -1]}
;;     {:p [0 0], :v [1 3]}
;;     {:p [3 0], :v [-2 -2]}
;;     {:p [7 6], :v [-1 -3]}
;;     {:p [3 0], :v [-1 -2]}
;;     {:p [9 3], :v [2 3]}
;;     {:p [7 3], :v [-1 2]}
;;     {:p [2 4], :v [2 -3]}
;;     {:p [9 5], :v [-3 -3]}],
;;    :positions #{[7 6] [0 0] [6 3] [7 3] [3 0] [9 3] [2 4] [2 0] [0 4] [9 5] [10 3]}}

(defn step-robot [[w h] {:keys [p v]} step-count]
  {:p [(mod
        (+ (first p) (* step-count (first v)))
        w)
       (mod
        (+ (second p) (* step-count (second v)))
        h)],
   :v v})

(step-robot [11 7] {:p [2 4], :v [2 -3]} 1)
;;=> {:p [4 1], :v [2 -3]}

(step-robot [11 7] {:p [2 4], :v [2 -3]} 2)
;;=> {:p [6 5], :v [2 -3]}

(step-robot [11 7] {:p [2 4], :v [2 -3]} 3)
;;=> {:p [8 2], :v [2 -3]}

(step-robot [11 7] {:p [2 4], :v [2 -3]} 4)
;;=> {:p [10 6], :v [2 -3]}

(step-robot [11 7] {:p [2 4], :v [2 -3]} 5)
;;=> {:p [1 3], :v [2 -3]}

(defn step-robots [{:keys [bounds robots]} steps]
  (update-positions {:bounds bounds :robots (mapv #(step-robot bounds % steps) robots)}))

(step-robots sample 100)
;;=> {:bounds [11 7],
;;    :robots
;;    [{:p [3 5], :v [3 -3]}
;;     {:p [5 4], :v [-1 -3]}
;;     {:p [9 0], :v [-1 2]}
;;     {:p [4 5], :v [2 -1]}
;;     {:p [1 6], :v [1 3]}
;;     {:p [1 3], :v [-2 -2]}
;;     {:p [6 0], :v [-1 -3]}
;;     {:p [2 3], :v [-1 -2]}
;;     {:p [0 2], :v [2 3]}
;;     {:p [6 0], :v [-1 2]}
;;     {:p [4 5], :v [2 -3]}
;;     {:p [6 6], :v [-3 -3]}],
;;    :positions #{[2 3] [5 4] [9 0] [6 6] [1 3] [4 5] [0 2] [1 6] [6 0] [3 5]}}


(defn safety-check [{:keys [bounds robots]}]
  (reduce *
          (let [mid-width (floor (/ (first bounds) 2))
                max-left (dec mid-width)
                min-right (inc mid-width)
                min-height (floor (/ (second bounds) 2))
                max-top (dec min-height)
                min-bottom (inc min-height)]
            (reduce
             (fn [sums, {[i j] :p}]
               (cond
                 (<= i max-left)
                 (cond
                   (<= j max-top) (update sums 0 inc)
                   (>= j min-bottom) (update sums 1 inc)
                   :else sums)

                 (>= i min-right)
                 (cond
                   (<= j max-top) (update sums 2 inc)
                   (>= j min-bottom) (update sums 3 inc)
                   :else sums)

                 :else sums)) [0 0 0 0] robots))))



(safety-check (step-robots sample 100))
;;=> 12
(safety-check (step-robots input 100))
;;=> 219150360



(defn display-robots [{[w h] :bounds robots :robots}]
  (let [empty-buffer (transient (vec (repeat (* h w) \.)))
        index (fn [[i j]] (+ (* w j) i))
        buffer (persistent! (reduce
                             (fn [buffer {[i j] :p}]
                               (assoc! buffer (index [i j]) \#))
                             empty-buffer robots))]
    (doseq [line (partition w buffer)]
      (println (apply str line)))))

(display-robots (step-robots input 2))

(defn robot-at-top [{[w _] :bounds robots :robots}]
  (let [mid-width (int (floor (/ w 2)))]
    (when-let [{[_ star-j] :p}  (some (fn [r] (when (= (get-in r [:p 0]) mid-width) r)) robots)]
      (println star-j (inc star-j) (inc (inc star-j)) (inc mid-width) (dec mid-width) (inc (inc mid-width)) (dec (dec mid-width)))
      (and
       (some (fn [{[i j] :p}] (and (= j (inc star-j)) (= i (inc mid-width)))) robots)
       (some (fn [{[i j] :p}] (and (= j (inc star-j)) (= i (dec mid-width)))) robots)
       (some (fn [{[i j] :p}] (and (= j (inc (inc star-j))) (= i (inc (inc mid-width))))) robots)
       (some (fn [{[i j] :p}] (and (= j (inc (inc star-j))) (= i (dec (dec mid-width))))) robots)))))


(defn any-tree-shape [{:keys [robots positions]}]
  (some
   (fn [{[star-i star-j] :p}]
     (and
      (contains? positions [(+ star-i 1), (+ star-j 1)])
      (contains? positions [(- star-i 1), (+ star-j 1)])
      (contains? positions [(+ star-i 2), (+ star-j 2)])
      (contains? positions [(- star-i 2), (+ star-j 2)])
      (contains? positions [(+ star-i 3), (+ star-j 3)])
      (contains? positions [(- star-i 3), (+ star-j 3)])
      (contains? positions [(+ star-i 4), (+ star-j 4)])
      (contains? positions [(- star-i 4), (+ star-j 4)])))
   robots))


(defn find-xmas [robots start]

  (loop [robots (step-robots robots start) steps start]
    (when (zero? (rem steps 100000)) (println steps))
    (cond
      (any-tree-shape robots)
      (do
        (println steps)
        (display-robots robots)
        steps)
      (= 500000 steps)
      (do
        (println "failed")
        nil)
      :else (recur (step-robots robots 1) (inc steps)))))

(robot-at-top {:bounds [101 103] :robots [{:p [50 2] :v [1 1]}
                                          {:p [51 3] :v [1 1]}
                                          {:p [49 3] :v [1 1]}
                                          {:p [52 4] :v [1 1]}
                                          {:p [48 4] :v [1 1]}]})

(any-tree-shape (update-positions {:bounds [101 103] :robots [{:p [50 2] :v [1 1]}
                                                              {:p [51 3] :v [1 1]}
                                                              {:p [49 3] :v [1 1]}
                                                              {:p [52 4] :v [1 1]}
                                                              {:p [48 4] :v [1 1]}
                                                              {:p [53 5] :v [1 1]}
                                                              {:p [47 5] :v [1 1]}]}))

(any-tree-shape {:bounds [101 103] :robots [{:p [51 2] :v [1 1]}
                                            {:p [52 3] :v [1 1]}
                                            {:p [50 3] :v [1 1]}
                                            {:p [53 4] :v [1 1]}
                                            {:p [49 4] :v [1 1]}]})



(find-xmas {:bounds [101 103] :robots [{:p [50 2] :v [1 1]}
                                       {:p [51 3] :v [1 1]}
                                       {:p [49 3] :v [1 1]}
                                       {:p [52 4] :v [1 1]}
                                       {:p [48 4] :v [1 1]}]} 0)

(defn greet [n]
  (find-xmas input n))



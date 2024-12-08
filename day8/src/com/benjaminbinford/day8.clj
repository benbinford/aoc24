(ns com.benjaminbinford.day8
  (:require [clojure.string :as str])
  (:gen-class))




(def input (str/split-lines (slurp "resources/input.txt")))
(def sample (str/split-lines (slurp "resources/sample.txt")))

(defn scan-to-map
  "Converts the 2d grid of characters into a map of value->[position]
   where position is the [x y] of the value in the grid, ignoring \\. values"
  [input]
  (let [h (count input)
        w (count (first input))]
    {:h h
     :w w
     :values (reduce
              (fn [acc [value pos]]
                (update acc value #(conj % pos)))
              {}
              (for [y (range h)
                    x (range w)
                    :let [v (get-in input [y x])]
                    :when (not= v \.)]
                [v [x y]]))}))

(scan-to-map sample)
;;=> {:h 12, :w 12, :values {\0 ([4 4] [7 3] [5 2] [8 1]), \A ([9 9] [8 8] [6 5])}}

(defn pair-wise
  "Return a sequence of all the unique combination of pairs of elements from input sequence. 
   For example, (pair-wise [x y z]) => ([x y] [x z] [y z]). If [x y] is in the result, [y x] will not be."
  [xs]
  (for [[i x] (map-indexed vector xs)
        [j y] (map-indexed vector xs)
        :when (< i j)]
    [x y]))


(pair-wise [1 2 3])
;;=> ([1 2] [1 3] [2 3])

(pair-wise '([4 4] [7 3] [5 2] [8 1]))
;;=> ([[4 4] [7 3]] [[4 4] [5 2]] [[4 4] [8 1]] [[7 3] [5 2]] [[7 3] [8 1]] [[5 2] [8 1]])

(defn expand-points
  "Given 2 points, take the delta between them and return the 2 points on on the line 
   between then either side of them delta away"
  [[x1 y1] [x2 y2] w h]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        x0 (- x1 dx)
        y0 (- y1 dy)
        x3 (+ x2 dx)
        y3 (+ y2 dy)]
    (filter (fn [[x y]] (and (< -1 x w) (< -1 y h)))
            [[x0 y0] [x3 y3]])))

(expand-points [9 9] [8 8] 12 12)
;;=> ([7 7] [10 10])

(expand-points [4 3] [8 4] 12 12)
;;=> ([0 2])

(defn find-nodals
  [points w h f]
  (into #{}
        (mapcat (fn [[p1 p2]] (f p1 p2 w h)) (pair-wise points))))

(find-nodals [[9 9] [8 8] [6 5]] 12 12 expand-points)
;;=> #{[7 7] [4 2] [10 11] [10 10] [3 1]}

(defn find-all-nodals
  [input f]
  (let [{:keys [h w values]} input]
    (into #{} (mapcat #(find-nodals % w h f) (vals values)))))


(find-all-nodals sample expand-points)
;;=> #{[10 5] [7 7] [4 2] [3 0] [6 6] [10 11] [11 3] [10 10] [2 0] [3 1] [9 5] [9 4] [1 2]}

(defn part1 [input]
  (count (find-all-nodals (scan-to-map input) expand-points)))

(part1 sample)
;;=> 14

(part1 input)
;;=> 367


(defn expand-all-points
  "Given 2 points, take the delta between them and return the 2 points on on the line 
   between then either side of them delta away"
  [[x1 y1] [x2 y2] w h]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (concat (loop [x0 x1
                   y0 y1
                   acc []]
              (if (and (< -1 x0 w) (< -1 y0 h))
                (recur (- x0 dx) (- y0 dy)  (conj acc [x0 y0]))
                acc))
            (loop [x3 x2
                   y3 y2
                   acc []]
              (if (and (< -1 x3 w) (< -1 y3 h))
                (recur (+ x3 dx) (+ y3 dy) (conj acc [x3 y3]))
                acc)))))

(find-all-nodals {:h 10 :w 10 :values {\T [[0 0] [3 1] [1 2]]}} expand-all-points)
;;=> #{[9 3] [4 8] [2 4] [3 6] [5 0] [6 2]}


(defn part2 [input]
  (count (find-all-nodals (scan-to-map input) expand-all-points)))


(part2 sample)
;;=> 34

(part2 input)
;;=> 1285



(defn print-map
  "Prints the map with the nodals marked with a #"
  [nodals h w]
  (doseq [y (range h)
          x (range w)]
    (print (if (contains? nodals [x y]) "#" "."))
    (when (= x (dec w))
      (println))))

(print-map (find-all-nodals {:h 10 :w 10 :values {\T [[0 0] [3 1] [1 2]]}} expand-all-points) 10 10)

(expand-points [8 1] [5 2] 12 12)
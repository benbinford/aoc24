(ns com.benjaminbinford.day12
  (:require [clojure.string :as str])
  (:gen-class))





(def input (str/split-lines (slurp "resources/input.txt")))
(def sample (str/split-lines (slurp "resources/sample.txt")))


(defn cref [input i j]
  (let [row (nth input j nil)]
    (if row
      (nth row i nil)
      nil)))


(cref sample 9 0)
;;=> \F
(cref sample 10 0)
;;=> nil
(cref sample 0 10)
;;=> nil
(cref sample 0 9)
;;=> \M

(defn cell-area [input i j]
  (if (nil? (cref input i j))
    0
    1))

(cell-area sample 9 0)
;;=> 1
(cell-area sample 10 0)
;;=> 0

(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(defn neighbors [input i j]
  (let [c (cref input i j)]
    (for [[di dj] dirs
          :let [ni (+ i di)
                nj (+ j dj)
                nc (cref input ni nj)]
          :when (not (nil? nc))]
      [(if (= c nc) :same :diff) ni nj])))

(defn same-neighbors [neighbors]
  (map #(into [] (rest %)) (filter #(= :same (first %)) neighbors)))

(defn diff-neighbors [neighbors]
  (map rest (filter #(= :diff (first %)) neighbors)))

(same-neighbors (neighbors sample 9 0))
;;=> ([9 1] [8 0])

(neighbors sample 9 0)
;;=> ([:same 9 1] [:same 8 0])
;

(neighbors sample 9 1)
;;=> ([:same 9 0] [:same 9 2] [:diff 8 1])

(neighbors sample 4 4)
;;=> ([:same 4 3] [:diff 5 4] [:same 4 5] [:diff 3 4])
;;=> ([4 3] [4 5])

(neighbors sample 6 4)
;;=> ([:same 6 3] [:diff 7 4] [:same 6 5] [:same 5 4])
;;=> ([6 3] [6 5] [5 4])

(defn cell-perimeter [neighbors]
  (- 4 (count (same-neighbors neighbors))))

(cell-perimeter (neighbors sample 9 0))
;;=> 2

(cell-perimeter (neighbors sample 9 1))
;;=> 2

(cell-perimeter (neighbors sample 3 7))
;;=> 0

(defn merge-region [r1 r2]
  {:top-left [(min (first (:top-left r1)) (first (:top-left r2))) (min (second (:top-left r1)) (second (:top-left r2)))]
   :bottom-right [(max (first (:bottom-right r1)) (first (:bottom-right r2))) (max (second (:bottom-right r1)) (second (:bottom-right r2)))]
   :area (+ (:area r1) (:area r2))
   :perimeter (+ (:perimeter r1) (:perimeter r2))
   :perimeters (merge (:perimeters r1) (:perimeters r2))
   :region (into (:region r1) (:region r2))})

(defn find-region
  "Return {:area area :perimeter perimeter :region #{[i1,j1] [i2,j2]}} 
   for the cells that have neighbors with the same value based at i,j
   The area is the sum of cell-area for all cells in the region
   the perimeter is the sum of cell-perimeter for all cells in the region
   The region is the list of cells in the region"
  ([input i j region]
   (if (contains? (:region region) [i j])
     region
     (let [neighbors (neighbors input i j)
           same (same-neighbors neighbors)
           new-region (merge-region region {:top-left [i j] :bottom-right [i j]
                                            :area (cell-area input i j)
                                            :perimeter (cell-perimeter neighbors)
                                            :perimeters {[i j] (cell-perimeter neighbors)}
                                            :region #{[i j]}})]
       (reduce (fn [region [i j]]
                 (find-region input i j region)) new-region same))))
  ([input i j]
   (find-region input i j {:top-left [i j] :bottom-right [i j] :area 0 :perimeter 0 :region #{}})))


(find-region sample 0 0)
;;=> {:top-left [0 0],
;;    :bottom-right [4 3],
;;    :area 12,
;;    :perimeter 18,
;;    :perimeters
;;    {[2 2] 1, [0 0] 2, [1 0] 1, [2 3] 3, [1 1] 1, [4 2] 3, [3 0] 2, [2 0] 1, [3 1] 1, [2 1] 0, [3 2] 1, [0 1] 2},
;;    :region #{[2 2] [0 0] [1 0] [2 3] [1 1] [4 2] [3 0] [2 0] [3 1] [2 1] [3 2] [0 1]}}




(find-region sample 4 0)
;;=> {:top-left [4 0],
;;    :bottom-right [5 1],
;;    :area 4,
;;    :perimeter 8,
;;    :perimeters {[4 0] 2, [5 0] 2, [5 1] 2, [4 1] 2},
;;    :region #{[4 1] [5 1] [5 0] [4 0]}}
;;=> {:area 4, :perimeter 8, :region #{[4 1] [5 1] [5 0] [4 0]}}

(find-region sample 6 0)
;;=> {:area 14,
;;    :perimeter 28,
;;    :region #{[7 1] [4 3] [3 3] [5 3] [5 2] [8 1] [6 1] [5 6] [5 5] [4 5] [7 0] [4 4] [6 2] [6 0]}}


(defn find-regions
  ([input seen-cells height width]

   (loop [j 0
          i 0
          regions []
          seen-cells seen-cells]
     (cond
       (>= j height)
       regions

       (>= i width)
       (recur (inc j) 0 regions seen-cells)

       (contains? seen-cells [i j])
       (recur j (inc i) regions seen-cells)

       :else
       (let [region (find-region input i j)
             new-seen-cells (into seen-cells (:region region))]
         (recur j (inc i) (conj regions region) new-seen-cells)))))

  ([input]
   (find-regions input #{} (count input) (count (first input)))))

(find-regions sample)
;;=> [{:area 12, :perimeter 18, :region #{[2 2] [0 0] [1 0] [2 3] [1 1] [4 2] [3 0] [2 0] [3 1] [2 1] [3 2] [0 1]}}
;;    {:area 4, :perimeter 8, :region #{[4 1] [5 1] [5 0] [4 0]}}
;;    {:area 14,
;;     :perimeter 28,
;;     :region #{[7 1] [4 3] [3 3] [5 3] [5 2] [8 1] [6 1] [5 6] [5 5] [4 5] [7 0] [4 4] [6 2] [6 0]}}
;;    {:area 10, :perimeter 18, :region #{[8 4] [7 2] [8 3] [7 3] [9 0] [9 3] [8 0] [8 2] [9 2] [9 1]}}
;;    {:area 13, :perimeter 20, :region #{[0 6] [0 5] [3 4] [1 4] [1 3] [1 5] [0 3] [2 4] [0 2] [0 4] [1 6] [1 2] [3 5]}}
;;    {:area 11, :perimeter 20, :region #{[7 6] [7 7] [6 7] [5 4] [6 3] [6 6] [6 5] [6 4] [6 8] [6 9] [7 5]}}
;;    {:area 1, :perimeter 4, :region #{[7 4]}}
;;    {:area 13, :perimeter 18, :region #{[8 8] [8 7] [9 8] [8 9] [8 6] [7 8] [9 6] [9 9] [8 5] [7 9] [9 7] [9 5] [9 4]}}
;;    {:area 14,
;;     :perimeter 22,
;;     :region #{[3 9] [2 8] [2 5] [4 7] [4 6] [5 7] [1 8] [1 7] [5 8] [2 7] [3 6] [3 8] [3 7] [2 6]}}
;;    {:area 5, :perimeter 12, :region #{[1 9] [2 9] [0 9] [0 7] [0 8]}}
;;    {:area 3, :perimeter 8, :region #{[4 9] [4 8] [5 9]}}]

(defn cost [regions]
  (reduce
   +
   (map #(* (:area %) (:perimeter %)) regions)))

(cost (find-regions sample))
;;=> 1930

(cost (find-regions input))
;;=> 1461752

(defn create-state [direction]
  {:in-vertical false
   :adj 0
   :total-adj 0
   :direction direction})

(defn next-state [region i j {:keys [in-vertical adj total-adj direction]}]

  (let [[di dj] direction
        cell (contains? region [i j])
        neighbor (contains? region [(+ i di) (+ j dj)])]
    (cond
      (or (not cell) (and cell neighbor))
      (do
        ;(println "not cell or (and cell neighbor) [i j] adj total-adj" [i j] adj total-adj)

        {:in-vertical false
         :adj 0
         :total-adj (+ total-adj adj)
         :direction direction})

      (and cell (not neighbor) in-vertical)
      (do
        ;(println "and cell (not neighbor) in-vertical [i j] adj total-adj" [i j] adj total-adj)
        {:in-vertical true
         :adj (inc adj)
         :total-adj total-adj
         :direction direction})

      :else ; (and cell (not neighbor) (not in-vertical))
      (do
        ;(println "else" [i j] adj total-adj)
        {:in-vertical true
         :adj 0
         :total-adj total-adj
         :direction direction}))))


(defn scan-verticals
  "returns the perimeter adjustment for all the vertical lines in the region"
  ([region i min-j max-j]
   (loop [j min-j

          left-state (create-state [-1 0])
          right-state (create-state [1 0])]
     (if (> j max-j)
       (+ (:total-adj left-state) (:total-adj right-state) (:adj left-state) (:adj right-state))
       (recur (inc j) (next-state (:region region) i j left-state) (next-state (:region region) i j right-state)))))

  ([region]
   (let [min-j (second (:top-left region))
         max-j (second (:bottom-right region))
         min-i (first (:top-left region))
         max-i (first (:bottom-right region))]
     (reduce + (map #(scan-verticals region % min-j max-j) (range min-i (inc max-i)))))))



(defn scan-horizontals
  "returns the perimeter adjustment for all the vertical lines in the region"
  ([region j min-i max-i]
   (loop [i min-i

          left-state (create-state [0 -1])
          right-state (create-state [0 1])]
     ;(println i left-state right-state)
     (if (> i max-i)
       (+ (:total-adj left-state) (:total-adj right-state) (:adj left-state) (:adj right-state))
       (recur (inc i) (next-state (:region region) i j left-state) (next-state (:region region) i j right-state)))))

  ([region]
   (let [min-j (second (:top-left region))
         max-j (second (:bottom-right region))
         min-i (first (:top-left region))
         max-i (first (:bottom-right region))]
     (reduce + (map #(scan-horizontals region % min-i max-i) (range min-j (inc max-j)))))))


(def sample2 (find-regions ["AAAA"
                            "BBCD"
                            "BBCC"
                            "EEEC"]))

sample2
;;=> [{:top-left [0 0],
;;     :bottom-right [3 0],
;;     :area 4,
;;     :perimeter 10,
;;     :perimeters {[0 0] 3, [1 0] 2, [2 0] 2, [3 0] 3},
;;     :region #{[0 0] [1 0] [3 0] [2 0]}}
;;    {:top-left [0 1],
;;     :bottom-right [1 2],
;;     :area 4,
;;     :perimeter 8,
;;     :perimeters {[0 1] 2, [1 1] 2, [1 2] 2, [0 2] 2},
;;     :region #{[1 1] [0 2] [1 2] [0 1]}}
;;    {:top-left [2 1],
;;     :bottom-right [3 3],
;;     :area 4,
;;     :perimeter 10,
;;     :perimeters {[2 1] 3, [2 2] 2, [3 2] 2, [3 3] 3},
;;     :region #{[2 2] [3 3] [2 1] [3 2]}}
;;    {:top-left [3 1], :bottom-right [3 1], :area 1, :perimeter 4, :perimeters {[3 1] 4}, :region #{[3 1]}}
;;    {:top-left [0 3],
;;     :bottom-right [2 3],
;;     :area 3,
;;     :perimeter 8,
;;     :perimeters {[0 3] 3, [1 3] 2, [2 3] 3},
;;     :region #{[2 3] [1 3] [0 3]}}]
(scan-verticals (first sample2))
;;=> 0

(scan-horizontals  (first sample2))
;;=> 6
(scan-verticals (second sample2))
;;=> 2

(scan-horizontals  (second sample2))
;;=> 2

(scan-verticals (nth sample2 2))
;;=> 0

(scan-horizontals  (nth sample2 2))
;;=> 0


(scan-verticals (nth sample2 3))
;;=> 0

(scan-horizontals  (nth sample2 3))
;;=> 0

(scan-verticals (nth sample2 4))
;;=> 0

(scan-horizontals  (nth sample2 4))
;;=> 4



(defn cost2 [regions]
  (reduce
   +
   (map #(* (:area %) (- (:perimeter %) (scan-horizontals %) (scan-verticals %))) regions)))

(cost2 sample2)
;;=> 80
(cost2 (find-regions sample))
;;=> 1206

(cost2 (find-regions input))
;;=> 904114

(ns com.benjaminbinford.day20
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map-by]])
  (:gen-class))





(defn make-vertex [j i]
  {:j j :i i})




(defn find-in-grid [grid c]
  (some (fn [[j row]]
          (some (fn [[i kind]]
                  (when (= kind c) (make-vertex j i)))
                (map-indexed (fn [idx itm] [idx itm]) row)))
        (map-indexed (fn [idx itm] [idx itm]) grid)))





(defn next-cell [grid current prior]
  (first (for [[dj di] [[1 0] [-1 0] [0 1] [0 -1]]
               :let [j (+ dj (:j current))
                     i (+ di (:i current))
                     v (get-in grid [j i])
                     next (make-vertex j i)]
               :when (and (#{\. \S \E} v)
                          (not= next prior))]
           next)))



(next-cell (str/split-lines (slurp "resources/sample.txt")) {:j 3 :i 1} nil)
;;=> {:j 2, :i 1}
(next-cell (str/split-lines (slurp "resources/sample.txt")) {:j 2 :i 1} {:j 3 :i 1})
;;=> {:j 1, :i 1}
;;=> {:j 3, :i 1}


(defn distances [{:keys [start end grid] :as m}]
  (loop [distance 0
         distances {}
         current start
         prior nil]
    (let [distances (assoc distances current distance)]
      (if (= current end)
        (assoc m :distances distances)
        (recur (inc distance)
               distances
               (next-cell grid current prior)
               current)))))

(defn graph-meta [input]
  (let [h (count input)
        w (count (first input))]
    (distances {:h h :w w :start (find-in-grid input \S) :end (find-in-grid input \E) :grid input})))




(defn parse [file]
  (graph-meta (str/split-lines (slurp file))))



(def input (parse "resources/input.txt"))
(def sample (parse "resources/sample.txt"))
;;=> #'com.benjaminbinford.day20/sample

(get-in sample [:distances {:j 7, :i 5}])
;;=> 84



sample
;;=> {:h 15,
;;    :w 15,
;;    :start {:j 3, :i 1},
;;    :end {:j 7, :i 5},
;;    :grid
;;    ["###############"
;;     "#...#...#.....#"
;;     "#.#.#.#.#.###.#"
;;     "#S#...#.#.#...#"
;;     "#######.#.#.###"
;;     "#######.#.#...#"
;;     "#######.#.###.#"
;;     "###..E#...#...#"
;;     "###.#######.###"
;;     "#...###...#...#"
;;     "#.#####.#.###.#"
;;     "#.#...#.#.#...#"
;;     "#.#.#.#.#.#.###"
;;     "#...#...#...###"
;;     "###############"],


(defn find-cheats [m max-length v]
  (let [starting-distance (get-in m [:distances v])]
    (for [dx (range (- max-length) (inc max-length))
          dy (range (- max-length) (inc max-length))
          :when (or (not= dx 0) (not= dy 0))
          :let [cheat-distance (+ (abs dx) (abs dy))]
          :when (<= cheat-distance max-length)
          :let [j (+ dx (:j v))
                i (+ dy (:i v))
                d (get-in m [:distances {:j j, :i i}])]
          :when d
          :let [savings (- d (+ starting-distance cheat-distance))]
          :when (> savings 0)]
      {:start v :end (make-vertex j i)  :savings savings})))
(get-in sample [:distances {:j 1, :i 7}])
;;=> 12
(range (- 2) (inc 2))
;;=> (-2 -1 0 1 2)


(find-cheats sample (make-vertex 1 7) 2)
;;=> ({:start {:j 1, :i 7}, :end {:j 1, :i 9}, :savings 12})

(find-cheats sample (make-vertex 7 9) 2)
;;=> ({:start {:j 7, :i 9}, :end {:j 7, :i 11}, :savings 20} {:start {:j 7, :i 9}, :end {:j 9, :i 9}, :savings 36})

(find-cheats sample (make-vertex 7 8) 2)
;;=> ({:start {:j 7, :i 8}, :end {:j 9, :i 8}, :savings 38})

(defn find-all-cheats [input max-length]
  (into #{} (mapcat (partial find-cheats input max-length) (keys (get input :distances)))))

(sort-by second >
         (map (fn [[savings list]] [(count list) savings])
              (group-by :savings (find-all-cheats sample 20))))
;;=> ([3 76]
;;    [4 74]
;;    [22 72]
;;    [12 70]
;;    [14 68]
;;    [12 66]
;;    [19 64]
;;    [20 62]
;;    [23 60]
;;    [25 58]
;;    [39 56]
;;    [29 54]
;;    [31 52]
;;    [32 50]
;;    [37 48]
;;    [38 46]
;;    [99 44]
;;    [41 42]
;;    [93 40]
;;    [51 38]
;;    [57 36]
;;    [58 34]
;;    [61 32]
;;    [61 30]
;;    [80 28]
;;    [66 26]
;;    [129 24]
;;    [76 22]
;;    [217 20]
;;    [94 18]
;;    [263 16]
;;    [101 14]
;;    [252 12]
;;    [109 10]
;;    [224 8]
;;    [122 6]
;;    [329 4]
;;    [138 2])
;;=> ([138 2]
;;    [329 4]
;;    [122 6]
;;    [224 8]
;;    [109 10]
;;    [252 12]
;;    [101 14]
;;    [263 16]
;;    [94 18]
;;    [217 20]
;;    [76 22]
;;    [129 24]
;;    [66 26]
;;    [80 28]
;;    [61 30]
;;    [61 32]
;;    [58 34]
;;    [57 36]
;;    [51 38]
;;    [93 40]
;;    [41 42]
;;    [99 44]
;;    [38 46]
;;    [37 48]
;;    [32 50]
;;    [31 52]
;;    [29 54]
;;    [39 56]
;;    [25 58]
;;    [23 60]
;;    [20 62]
;;    [19 64]
;;    [12 66]
;;    [14 68]
;;    [12 70]
;;    [22 72]
;;    [4 74]
;;    [3 76])
;;=> ([14 2] [14 4] [2 6] [4 8] [2 10] [3 12] [1 20] [1 36] [1 38] [1 40] [1 64])
;;   



(reduce +
        0
        (map first (filter #(>= (second %) 100)
                           (map (fn [[savings list]] [(count list) savings])
                                (group-by :savings (find-all-cheats input 2))))))
;;=> 1197



(defn part2 [input min-cheat max-length]

  (reduce +
          0
          (map first (filter #(>= (second %) min-cheat)
                             (map (fn [[savings list]] [(count list) savings])
                                  (group-by :savings (find-all-cheats input max-length)))))))


(part2 input 100 20)
;;=> 944910
;;=> 1197
(reduce +
        0
        (map first (filter #(>= (second %) 50)
                           (map (fn [[savings list]] [(count list) savings])
                                (group-by :savings (find-all-cheats input 20))))))
;;=> 1199522
;;=> 285
;;=> 1197
       
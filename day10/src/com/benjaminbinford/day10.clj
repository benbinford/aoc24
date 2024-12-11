(ns com.benjaminbinford.day10
  (:require [clojure.string :as str])
  (:gen-class))





(def input (str/split-lines (slurp "resources/input.txt")))
(def sample (str/split-lines (slurp "resources/sample.txt")))


(defn next-path [c]
  (cond
    (= c \0) \1
    (= c \1) \2
    (= c \2) \3
    (= c \3) \4
    (= c \4) \5
    (= c \5) \6
    (= c \6) \7
    (= c \7) \8
    (= c \8) \9
    :else nil))

(next-path \0)
;;=> \1

(next-path \9)
;;=> nil

(defn cref [input i j]
  (nth (nth input j) i))

(defn find-trailheads [input]
  (for [j (range (count input))
        i (range (count (first input)))
        :when (= (cref input i j) \0)]
    [i j]))

(find-trailheads sample)
;;=> ([2 0] [4 0] [4 2] [6 4] [2 5] [5 5] [0 6] [6 6] [1 7])

(def find-path
  (memoize
   (fn [input [i j]]
     (let [c (cref input i j)
           next-c (next-path c)]
       (if (= \9 c)
         (do ;(println "found summit [i j]" [i j])
           #{[i j]})
         (reduce #(into %1 %2) #{}
                 (for [[di dj] [[0 -1] [1 0] [0 1] [-1 0]]
                       :let [ni (+ i di)
                             nj (+ j dj)]
                       :when (and (>= ni 0) (< ni (count (first input)))
                                  (>= nj 0) (< nj (count input))
                                  (= (cref input ni nj) next-c))]
                   (do ;(println "considering [ni nj] nexst-c" [ni nj] next-c)
                     (find-path input [ni nj])))))))))

(defn score [input]
  (reduce +
          (for [[i j] (find-trailheads input)]
            (count (find-path input [i j])))))

(find-path sample [2 0])
;;=> #{[4 3] [1 0] [5 4] [0 3] [4 5]}
;;=> #{#{#{#{#{#{#{#{#{#{}}}}}}}}}}
(score sample)
;;=> 36
;;=> 36
;;=> 12
(score input)
;;=> 587



(def find-path2
  (memoize
   (fn [input [i j]]
     (let [c (cref input i j)
           next-c (next-path c)]
       (if (= \9 c)
         (do ;(println "found summit [i j]" [i j])
           [[i j]])
         (reduce #(into %1 %2) []
                 (for [[di dj] [[0 -1] [1 0] [0 1] [-1 0]]
                       :let [ni (+ i di)
                             nj (+ j dj)]
                       :when (and (>= ni 0) (< ni (count (first input)))
                                  (>= nj 0) (< nj (count input))
                                  (= (cref input ni nj) next-c))]
                   (do ;(println "considering [ni nj] nexst-c" [ni nj] next-c)
                     (find-path2 input [ni nj])))))))))



(defn score2 [input]
  (reduce +
          (for [[i j] (find-trailheads input)]
            (count (find-path2 input [i j])))))

(def sample2 (str/split-lines
              ".....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9...."))


(find-path2 sample2 [5 0])
;;=> [[2 6] [2 6] [2 6]]
;;=> [[2 6]]
;;=> [[2 6]]
;;=> [[4 3] [1 0] [5 4] [0 3] [4 5] [4 3] [1 0] [5 4] [0 3] [4 5]]
;;=> #{[4 3] [1 0] [5 4] [0 3] [4 5]}

(score2 sample)
;;=> 81
;;=> 51
(score2 input)
;;=> 1340

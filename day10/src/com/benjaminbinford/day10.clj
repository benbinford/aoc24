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
  (do
    (fn [input [i j] creator]
      (let [c (cref input i j)
            next-c (next-path c)]
        (if (= \9 c)
          (do ;(println "found summit [i j]" [i j])
            (creator [i j]))
          (reduce #(into %1 %2) (creator)
                  (for [[di dj] [[0 -1] [1 0] [0 1] [-1 0]]
                        :let [ni (+ i di)
                              nj (+ j dj)]
                        :when (and (>= ni 0) (< ni (count (first input)))
                                   (>= nj 0) (< nj (count input))
                                   (= (cref input ni nj) next-c))]
                    (do ;(println "considering [ni nj] nexst-c" [ni nj] next-c)
                      (find-path input [ni nj] creator)))))))))

(defn score [input creator]
  (reduce +
          (for [[i j] (find-trailheads input)]
            (count (find-path input [i j] creator)))))

(find-path sample [2 0] hash-set)
;;=> #{[4 3] [1 0] [5 4] [0 3] [4 5]}
;;=> Execution error (ArityException) at com.benjaminbinford.day10/fn (REPL:52).
;;   Wrong number of args (0) passed to: clojure.core/set
;;   
;;=> [[4 3]
;;    [5 4]
;;    [4 5]
;;    [1 0]
;;    [0 3]
;;    [4 3]
;;    [5 4]
;;    [4 5]
;;    [1 0]
;;    [0 3]
;;    [4 3]
;;    [5 4]
;;    [4 5]
;;    [1 0]
;;    [0 3]
;;    [4 3]
;;    [5 4]
;;    [4 5]
;;    [1 0]
;;    [0 3]]
;;=> #{[4 3] [1 0] [5 4] [0 3] [4 5]}
;;=> #{#{#{#{#{#{#{#{#{#{}}}}}}}}}}
(score sample hash-set)
;;=> 36
;;=> 36
;;=> 12
(score input hash-set)
;;=> 587



(def sample2 (str/split-lines
              ".....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9...."))


(find-path sample2 [5 0] vector)
;;=> [[2 6] [2 6] [2 6]]
;;=> [[2 6] [2 6] [2 6]]
;;=> [[2 6]]
;;=> [[2 6]]
;;=> [[4 3] [1 0] [5 4] [0 3] [4 5] [4 3] [1 0] [5 4] [0 3] [4 5]]
;;=> #{[4 3] [1 0] [5 4] [0 3] [4 5]}

(score sample vector)
;;=> 81
(score input vector)
;;=> 1340

(ns com.benjaminbinford.day4
  (:require [clojure.string :as str])
  (:gen-class))





(def input (str/split-lines (slurp "resources/input.txt")))
(def sample (str/split-lines (slurp "resources/sample.txt")))

(defn sref [s i j]
  (try
    (nth (nth s j) i)
    (catch IndexOutOfBoundsException _e
      \0)))

(sref sample 0 0)
;;=> \M
(sref sample -1 -1)
;;=> \0
(nth input 0)
;;=> "XXASMSMSXMXSMMXMAXMMMAMSAMASMSAMXMAXSXMASMAMMXXMASASXAMAMXSXMMSXSAMXSSXXMXAAXMAXMXXSXMXMMAAMMMMMMMMMMXXMSMSSSMXAAMAMMMSAMXXXXSMSAXMMXXMASMSS"

(def dirs [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn sref-from-base [s i j [di dj] dist]
  (sref s (+ i (* di dist)) (+ j (* dj dist))))

(sref-from-base sample 0 0 [1 1] 1)
;;=> \S
(sref-from-base sample 5 2 [-1 1] 2)
;;=> \S

(defn count-matches
  "Count the number of matches in the 8 directions from the given positio for the word XMAS"
  [s]
  (let [h (count s)
        w (count (nth s 0))]
    (reduce + (for [i (range w)
                    j (range h)
                    dir dirs
                    :when (and (= \X (sref s i j))
                               (= \M (sref-from-base s i j dir 1))
                               (= \A (sref-from-base s i j dir 2))
                               (= \S (sref-from-base s i j dir 3)))]
                1))))

(count-matches sample)
;;=> 18


(count-matches input)
;;=> 2569


(defn count-cross-matches
  "Count the number of matches in the 8 directions from the given positio for the words XMAS crossed diagonally"
  [s]
  (let [h (count s)
        w (count (nth s 0))]
    (reduce + (for [i (range w)
                    j (range h)
                    :when (and (or (and (= \M (sref s i j))
                                        (= \A (sref-from-base s i j [1 1] 1))
                                        (= \S (sref-from-base s i j [1 1] 2)))
                                   (and (= \S (sref s i j))
                                        (= \A (sref-from-base s i j [1 1] 1))
                                        (= \M (sref-from-base s i j [1 1] 2))))
                               (or (and (= \M (sref-from-base s i j [1 0] 2))
                                        (= \A (sref-from-base s i j [1 1] 1))
                                        (= \S (sref-from-base s i j [0 1] 2)))
                                   (and (= \S (sref-from-base s i j [1 0] 2))
                                        (= \A (sref-from-base s i j [1 1] 1))
                                        (= \M (sref-from-base s i j [0 1] 2)))))]
                1))))


(count-cross-matches sample)
;;=> 9

(count-cross-matches input)
;;=> 1998

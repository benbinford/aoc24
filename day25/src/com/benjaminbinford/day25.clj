(ns com.benjaminbinford.day25
  (:require [clojure.string :as str]) ; added clojure regex package
  (:gen-class))



(defn find-lock-heights [lines]
  (loop [heights []
         i 0
         j 1]
    (cond
      (>= i (count (first lines))) heights
      (= \. (nth (nth lines j) i)) (recur (conj heights (dec j)) (inc i) 1)
      :else (recur heights i (inc j)))))


(defn find-key-heights [lines]
  (loop [heights []
         i 0
         j 1]
    (cond
      (>= i (count (first lines))) heights
      (= \# (nth (nth lines j) i)) (recur (conj heights (- 6 j)) (inc i) 1)
      :else   (recur heights i (inc j)))))

(defn parse-key-or-lock
  "Parse a block of input that looks like
#####
.####
.####
.####
.#.#.
.#...
.....

   or
.....
#....
#....
#...#
#.#.#
#.###
#####

The locks are schematics that have the top row filled (#) and the bottom row empty (.); the keys have the top row empty and the bottom row filled.

For locks, those are the pins themselves; you can convert the pins in schematics to a list of heights, one per column. For keys, the columns make up the shape of the key where it aligns with pins; those can also be converted to a list of heights.

So, you could say the first lock has pin heights 0,5,3,4,3:

#####
.####
.####
.####
.#.#.
.#...
.....
Or, that the first key has heights 5,0,2,1,3:

.....
#....
#....
#...#
#.#.#
#.###
#####   
   "
  [{:keys [keys locks]} block]
  (let [lines (str/split block #"\n")]
    (if (re-find #"\." (first lines))
      {:keys (conj keys (find-key-heights lines)) :locks locks}
      {:keys keys :locks (conj locks (find-lock-heights lines))})))


(defn parse [input]
  (reduce parse-key-or-lock
          {:keys [] :locks []}
          (str/split input #"\n\n")))


(def sample (parse (slurp "resources/sample.txt")))
(def input (parse (slurp "resources/input.txt")))

(defn part1 [{:keys [keys locks]}]
  (reduce + (for [key keys
                  lock locks
                  :let [fit (map + key lock)]
                  :when (every? #(<= % 5) fit)]
              1)))

(part1 input)
;;=> 2840
;;=> 3
;;=> Syntax error compiling at (src/com/benjaminbinford/day25.clj:91:1).
;;   Unable to resolve symbol: part in this context
;;   
  

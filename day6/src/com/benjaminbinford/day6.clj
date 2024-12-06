(ns com.benjaminbinford.day6
  (:require [clojure.string :as str])
  (:gen-class))



(def input (str/split-lines (slurp "resources/input.txt")))
(def sample (str/split-lines (slurp "resources/sample.txt")))
(def loop-example (str/split-lines (slurp "resources/loop.txt")))

(def dirs (cycle [[0 -1] [1 0] [0 1] [-1 0]]))

(take 10 dirs)

(defn- sref [s i j]
  (nth (nth s j) i))

(defn- sref-proxy [override-i override-j value]
  (fn [s i j]
    (if (and (= i override-i)
             (= j override-j))
      value
      (sref s i j))))

(defn find-guard [s]
  (let [h (count s)
        w (count (nth s 0))]
    (first (for [i (range w)
                 j (range h)
                 :when (= \^ (sref s i j))]
             [i j]))))

(find-guard sample)
;;=> [4 6]

(defn wander-guard [s sref]
  (let [h (count s)
        w (count (nth s 0))
        pos (find-guard s)]
    (loop [pos pos
           dir (first dirs)
           dirs (rest dirs)
           visited #{pos}
           states #{(conj pos dir)}]

      (let [next-i (+ (first pos) (first dir))
            next-j (+ (second pos) (second dir))]
        (cond
          (or (< next-i 0)
              (< next-j 0)
              (>= next-i w)
              (>= next-j h)) (count visited)
          (= \# (sref s next-i next-j)) (recur pos (first dirs) (rest dirs) visited states)
          (contains? states [next-i next-j dir]) -1
          :else (recur [next-i next-j] dir dirs (conj visited [next-i next-j]) (conj states [next-i next-j dir])))))))



(wander-guard sample sref)
;;=> 41
;;=> 41
(wander-guard input sref)
;;=> 4722
(wander-guard loop-example sref)
;;=> -1

(wander-guard sample (sref-proxy 0 0 \#))
;;=> 41

(wander-guard sample (sref-proxy 3 6 \#))
;;=> -1

(defn part2 [s]
  (let [h (count s)
        w (count (nth s 0))]
    (reduce + (for [i (range w)
                    j (range h)
                    :when (and (not= \# (sref s i j)) (not= \^ (sref s i j)))]
                (let [step-count (wander-guard s (sref-proxy i j \#))]

                  (if (> step-count 0)
                    0
                    1))))))

(part2 sample)
;;=> 6

(part2 input)
;;=> 1602
;;=> 1602

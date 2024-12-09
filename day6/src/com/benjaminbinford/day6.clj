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

(defn wander-guard
  ([s ref]
   (wander-guard s ref (find-guard s)))
  ([s sref pos]
   (let [h (count s)
         w (count (nth s 0))]
     (loop [pos pos
            dir (first dirs)
            dirs (rest dirs)
            states #{(conj pos dir)}]

       (let [next-i (+ (nth pos 0) (first dir))
             next-j (+ (nth pos 1) (second dir))]
         (cond
           (or (< next-i 0)
               (< next-j 0)
               (>= next-i w)
               (>= next-j h)) {:status :exit :visited (fn [] (into #{} (map #(take 2 %) states)))}
           (= \# (sref s next-i next-j)) (recur pos (first dirs) (rest dirs) states)
           (contains? states [next-i next-j dir]) {:status :loop}
           :else (recur [next-i next-j] dir dirs  (conj states [next-i next-j dir]))))))))



(count ((:visited (wander-guard sample sref))))
;;=> 41
;;=> 41
(count ((:visited (wander-guard input sref))))
;;=> 4722
(wander-guard loop-example sref)
;;=> {:status :loop}

(count ((:visited (wander-guard sample (sref-proxy 0 0 \#)))))
;;=> 41

(wander-guard sample (sref-proxy 3 6 \#))
;;=> {:status :loop}

(defn part2 [s]
  (let [guard-pos (find-guard s)
        result (wander-guard s sref guard-pos)
        visited ((:visited result))]
    (reduce + (for [[i j] visited
                    :when (not= guard-pos [i j])]
                (let [result (wander-guard s (sref-proxy i j \#)  guard-pos)]
                  (if (= :loop (:status result))
                    1
                    0))))))

(part2 sample)
;;=> 6

(part2 input)
;;=> 1602
;;=> 1602

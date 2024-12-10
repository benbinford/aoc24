(ns com.benjaminbinford.day9
  (:require [clojure.string :as str])
  (:gen-class))

(def input (first (str/split-lines (slurp "resources/input.txt"))))
(def sample (first (str/split-lines (slurp "resources/sample.txt"))))

(defn parse-input
  "Parse the string alternating between adding locations to a vector representing the 
   disk and a vector representing a free list. Return the disk and free list. 
   A disk map like 12345 would represent a one-block file, two blocks of free space, 
   a three-block file, four blocks of free space, and then a five-block file. 
   A disk map like 90909 would represent three nine-block files in a row 
   (with no free space between them)"
  [input]
  (loop [i input
         state :disk
         disk []
         free []
         offset 0
         file-id 0]
    (if (empty? i)
      {:disk (vec disk) :free (vec free)}
      (let [n (read-string (str (first i)))
            next-offset (+ offset n)]
        (if (= :disk state)
          (recur (rest i) :free  (into disk   (repeat n file-id)) free next-offset (inc file-id))
          (recur (rest i) :disk  (into disk   (repeat n nil)) (into free (range offset next-offset)) next-offset file-id))))))
(count input)
(parse-input sample)
;;=> {:disk
;;    [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil 7 7 7 nil 8 8 8 8 9 9],
;;    :free [2 3 4 8 9 10 12 13 14 18 21 26 31 35]}

(parse-input "12345")
;;=> {:disk [0 nil nil 1 1 1 nil nil nil nil 2 2 2 2 2], :free [1 2 6 7 8 9]}


(defn next-offset
  "Given an offset, decrement until a non-nil value is found and return the offset"
  [disk offset]
  (loop [offset (dec offset)]
    (if (nil? (nth disk offset))
      (recur (dec offset))
      offset)))

(defn compact
  "Compact the disk and free list by removing nils from the disk and updating the free list"
  [{:keys [disk free]}]
  (let [final-length (count (filter #(not (nil? %)) disk))]
    (loop [free free
           disk disk
           offset (dec (count disk))]
      (if (empty? free)
        (take final-length disk)
        (recur (rest free)
               (assoc disk (first free) (nth disk offset))
               (next-offset disk offset))))))

(compact (parse-input "12345"))
;;=> (0 2 2 1 1 1 2 2 2)

(compact (parse-input sample))
;;=> (0 0 9 9 8 1 1 1 8 8 8 2 7 7 7 3 3 3 6 4 4 6 5 5 5 5 6 6)

(defn part1 [input]
  (reduce + (map-indexed (fn [idx item] (* idx item)) (compact (parse-input input)))))

(part1 sample)
;;=> 1928

(part1 input)
;;=> 6415184586041
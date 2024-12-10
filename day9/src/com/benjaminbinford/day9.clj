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



(defn parse-input2
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
      {:disk disk :free free}
      (let [n (read-string (str (first i)))
            next-offset (+ offset n)]
        (if (= :disk state)
          (recur (rest i) :free  (into disk  [{:file-id file-id :length n :offset offset}]) free next-offset (inc file-id))
          (recur (rest i) :disk  disk (into free [{:length n :offset offset}]) next-offset file-id))))))


(parse-input2 sample)
;;=> {:disk
;;    [{:file-id 0, :length 2, :offset 0}
;;     {:file-id 1, :length 3, :offset 5}
;;     {:file-id 2, :length 1, :offset 11}
;;     {:file-id 3, :length 3, :offset 15}
;;     {:file-id 4, :length 2, :offset 19}
;;     {:file-id 5, :length 4, :offset 22}
;;     {:file-id 6, :length 4, :offset 27}
;;     {:file-id 7, :length 3, :offset 32}
;;     {:file-id 8, :length 4, :offset 36}
;;     {:file-id 9, :length 2, :offset 40}],
;;    :free
;;    [{:length 3, :offset 2}
;;     {:length 3, :offset 8}
;;     {:length 3, :offset 12}
;;     {:length 1, :offset 18}
;;     {:length 1, :offset 21}
;;     {:length 1, :offset 26}
;;     {:length 1, :offset 31}
;;     {:length 1, :offset 35}
;;     {:length 0, :offset 40}]}


(defn find-free-offset
  "Find the first index into the free list that is at least as long as the length of the file"
  [free length offset]
  (first (keep-indexed
          #(when
            (and (>= (:length %2) length) (< (:offset %2) offset))
             %1)
          free)))

(find-free-offset (:free (parse-input2 sample)) 4 40)
;;=> nil
(find-free-offset (:free (parse-input2 "12345")) 4 40)
;;=> 1
(find-free-offset (:free (parse-input2 "12345")) 4 3)
;;=> nil

(defn compact2
  "Compact the disk and free list by removing nils from the disk and updating the free list"
  [{:keys [disk free]}]
  (loop [free free
         disk disk
         file-index (dec (count disk))]
    (if (< file-index 1)
      disk
      (let [f (nth disk file-index)
            free-offset (find-free-offset free (:length f) (:offset f))
            free-block (if (nil? free-offset) nil (nth free free-offset))]
        (if (nil? free-offset)
          (recur free disk (dec file-index))

          (recur (update free free-offset #(update (update % :length - (:length f))
                                                   :offset + (:length f)))
                 (update-in disk [file-index :offset] (fn [_] (:offset free-block)))
                 (dec file-index)))))))



(compact2 (parse-input2 sample))
;;=> Execution error (NullPointerException) at com.benjaminbinford.day9/compact2 (REPL:149).
;;   Cannot invoke "java.lang.Character.charValue()" because "x" is null
;;   

(defn file-checksum [{:keys [file-id length offset]}]
  (reduce + (map #(* file-id %) (range offset (+ offset length)))))


(file-checksum {:file-id 2 :length 3 :offset 5})
;;=> 36

(defn part2 [input]
  (reduce + (map file-checksum (compact2 (parse-input2 input)))))



(part2 sample)
;;=> 2858
(part2 input)
;;=> 6436819084274
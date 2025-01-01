(ns com.benjaminbinford.day22
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))


(defn mix-and-prune [[secret value]]
  (mod (bit-xor secret value) 16777216))


(defn mul [secret value]
  [secret (* secret value)])

(defn div [secret value]
  [secret (quot secret value)])

(defn evolve-secret [secret]

  (-> secret
      (mul 64)
      mix-and-prune
      (div 32)
      mix-and-prune
      (mul 2048)
      mix-and-prune))

(evolve-secret 123)

(defn evolve-secrets [secret]
  (iterate evolve-secret  secret))
;; (take 10 (evolve-secrets 123))
;; ;;=> (15887950 16495136 527345 704524 1553684 12683156 11100544 12249484 7753432 5908254)


;; (take 10 (evolve-secrets 123))

;; (nth  (evolve-secrets 123) 10)

;; (nth  (evolve-secrets 1) 2000)


;; (reduce + 0 (map #(nth (evolve-secrets %) 2000) [1 10 100 2024]))
;; ;;=> 37327623


(def sample [1 10 100 2024])

(def input (read-string (slurp "resources/input.txt")))

(defn part1 [input]
  (reduce + 0 (map #(nth (evolve-secrets %) 2000) input)))




;; (part1 sample)
;;=> 37327623

;; (part1 input)
;;=> 12759339434


(def possible-diffs [9 8 7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9])
(def possible-seqs v)

(defn prices [initial mx]
  (take mx (map #(mod % 10) (evolve-secrets initial))))

;;(prices 123 10)
;;=> (3 0 6 5 4 4 6 4 4 2)

(defn buy-after-seq
  ([[p1 p2 p3 p4 p5 & ps] [s1 s2 s3 s4]]
   (buy-after-seq p1 p2 p3 p4 p5 ps s1 s2 s3 s4))
  ([p1 p2 p3 p4 p5 ps s1 s2 s3 s4]
   (let [d1 (- p2 p1)
         d2 (- p3 p2)
         d3 (- p4 p3)
         d4 (- p5 p4)]
     (cond (and (= d1 s1)
                (= d2 s2)
                (= d3 s3)
                (= d4 s4))
           p5

           (empty? ps)
           0

           :else
           (recur p2 p3 p4 p5 (first ps) (rest ps) s1 s2 s3 s4)))))


(defn add-if-not-present [m key value]
  (if (contains? m key)
    m
    (assoc m key value)))

(defn possible-buys
  ([[p1 p2 p3 p4 p5 & ps]]
   (possible-buys p1 p2 p3 p4 p5 ps {}))
  ([p1 p2 p3 p4 p5 ps acc]
   (let [d1 (- p2 p1)
         d2 (- p3 p2)
         d3 (- p4 p3)
         d4 (- p5 p4)
         key [d1 d2 d3 d4]]
     (if (empty? ps)
       acc
       (recur p2 p3 p4 p5 (first ps) (rest ps) (add-if-not-present acc key p5))))))

;;=> #'com.benjaminbinford.day22/buy-after-seq
;;=> #'com.benjaminbinford.day22/buy-after-seq

;;(buy-after-seq (prices 123 10) [-1,-1,0,2])
;;=> 6

;;(buy-after-seq (prices 1 2000) [-2,1,-1,3])
;;=> 7
;;(buy-after-seq (prices 2 2000) [-2,1,-1,3])
  ;;=> 7
;;(buy-after-seq (prices 3 2000) [-2,1,-1,3])
  ;;=> 9




;; (defn total-bananans [trial-seq all-prices]
;;   (reduce + 0 (map #(buy-after-seq % trial-seq) all-prices)))

;;(total-bananans [-2 1 -1 3] (map #(prices % 2000) [1 2 3 2024]))
;;=> 23

;; (defn find-best-seq [all-prices]
;;     (first (sort > (map (fn [seq] (total-bananans seq all-prices))
;;                         possible-seqs))))

(def sample-prices (map #(possible-buys (prices % 2000)) [1 2 3 2024]))
(def input-prices (map #(possible-buys (prices % 2000)) input))


(defn available-sequences [prices]
  (reduce #(into %1 %2) #{} (map #(keys %) prices)))
(defn total-bananans [seq all-prices]
  (reduce + 0 (map #(get % seq 0) all-prices)))

(defn find-best-price [prices]
  (let [seqs (available-sequences prices)]
    (first (sort > (map (fn [seq] (total-bananans seq prices))
                        seqs)))))


;(find-best-price sample-prices)
;;=> 23
;(find-best-price input-prices)
;;=> 1405

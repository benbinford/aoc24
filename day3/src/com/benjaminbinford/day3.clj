(ns com.benjaminbinford.day3
  (:require [clojure.string :as str])
  (:gen-class))




(def input (slurp "resources/input.txt"))
(def sample (slurp "resources/sample.txt"))
(def sample2 (slurp "resources/sample2.txt"))


(defn parse
  "Match all strings of the form mul(x,y) using a regex and return the list of pairs"
  [i]
  (map (fn [[_ x y]] [(Integer/parseInt x) (Integer/parseInt y)]) (re-seq #"mul\((\d+),(\d+)\)" i)))


(parse "mul(1,2) mul(3,4)")
;;=> ([1 2] [3 4])

(parse sample)
;;=> ([2 4] [5 5] [11 8] [8 5])


(defn part1 [i]
  (reduce + (map (fn [[x y]] (* x y)) (parse i))))

(part1 sample)
;;=> 161

(part1 input)
;;=> 185797128

(defn preprocess
  "remove all substrings starting with don't() and ending with end of string or do(). "
  ([s] (preprocess s :do [""]))
  ([s state acc]
   ; if we are in state :do, scan forward until end of string 
   ; or until we find dont(). add the substring to acc and recurse 
   ; changing state to don't
   ; if we are in state :dont, scan forward until end of string or do()
   ; discard substring and recurse
   (cond
     (= state :do)
     (let [i (str/index-of s "don't()")]
       (if (nil? i)
         (str/join "" (conj acc s))
         (recur (subs s (+ i 6)) :dont (conj acc (subs s 0 i)))))
     (= state :dont)
     (let [i (str/index-of s "do()")]
       (if (nil? i)
         (str/join "" acc)
         (recur (subs s (+ i 3)) :do acc))))))

(preprocess "don't() mul(1,2) do() mul(3,4)")
;;=> " mul(3,4)"

(preprocess sample)
;;=> "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))\n"

(preprocess sample2)
;;=> "xmul(2,4)&mul[3,7]!^?mul(8,5))\n"

(defn part2 [i]
  (part1 (preprocess i)))

(part2 sample2)
;;=> 48

(part2 input)
;;=> 89798695

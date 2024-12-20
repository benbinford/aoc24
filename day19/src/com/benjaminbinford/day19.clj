(ns com.benjaminbinford.day19
  (:require [clojure.string :as str])
  (:gen-class))





(defn parse [file]
  (let [[rs lines] (str/split (slurp file) #"\n\n")]
    {:rules (str/split rs #", ")
     :messages (str/split-lines lines)}))


(def input (parse "resources/input.txt"))
(def sample (parse "resources/sample.txt"))

(defn build-regex [rs]
  (re-pattern
   (str "^("  (str/join "|" (sort-by #(- (count %)) rs))  ")+$")))


(defn day1 [input]
  (let [rx (build-regex (get input :rules))]
    (count (keep #(re-matches rx %) (get input :messages)))))

(day1 input)

(re-matches (build-regex (get sample :rules)) "ubwu")
sample
;;=> {:rules ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"],
;;    :messages ["brwrr" "bggr" "gbbr" "rrbgbr" "ubwu" "bwurrg" "brgr" "bbrgwb"]}
(build-regex (get sample :rules))
;;=> #"(bwu|wr|rb|gb|br|r|b|g)+"
;;=> #"(r|b|g|wr|rb|gb|br|bwu)+"
;;=> Execution error (IllegalArgumentException) at com.benjaminbinford.day19/build-regex (REPL:22).
;;   Don't know how to create ISeq from: clojure.core$count
;;   


(defn fmatch [pattern]
  (fn [s]
    (when (str/starts-with? s pattern)
      (subs s (count pattern)))))

(defn build-func-matcher [input]
  (let [fs (map fmatch (:rules input))
        f (memoize
           (fn [f s]
             (if (empty? s)
               1
               (reduce
                +
                (map #(let [m (% s)]
                        (if m
                          (f f m)
                          0))
                     fs)))))]


    (fn [s] (f f s))))



(defn day2 [input]
  (let [f (build-func-matcher input)]
    (reduce + (map f (get input :messages)))))

(day2 input)
;;=> 606411968721181
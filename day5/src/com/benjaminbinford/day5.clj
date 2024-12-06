(ns com.benjaminbinford.day5
  (:require [clojure.string :as str])
  (:gen-class))





(def input (str/split-lines (slurp "resources/input.txt")))
(def sample (str/split-lines (slurp "resources/sample.txt")))


(defn index-of [pred coll]
  (first (keep-indexed (fn [idx x] (when (pred x) idx)) coll)))

(defn build-db
  "Given a list of X|Y rules, return a map of sets of Y for each X
   {Y1 #{X1 X2 X3}, Y2 #{X4 X5 X6}}"
  [rules]
  (reduce (fn [db [x y]]
            (update db y #(conj (or % #{}) x)))
          {}
          (map #(str/split % #"[|]") rules)))

(defn split-to-rules-and-updates
  "Split the input at the first empty line into rules and updates"
  [i]
  (let [idx (index-of str/blank? i) ; Find the index of the first empty line
        [r u] (split-at idx i)]
    [(build-db r) (map #(str/split  %  #",") (rest u))]))
(split-to-rules-and-updates sample)
;;=> [{"53" #{"61" "47" "75" "97"},
;;     "13" #{"61" "47" "53" "75" "97" "29"},
;;     "61" #{"47" "75" "97"},
;;     "47" #{"75" "97"},
;;     "29" #{"61" "47" "53" "75" "97"},
;;     "75" #{"97"}}
;;    (["75" "47" "61" "53" "29"]
;;     ["97" "61" "53" "29" "13"]
;;     ["75" "29" "13"]
;;     ["75" "97" "47" "61" "53"]
;;     ["61" "13" "29"]
;;     ["97" "13" "75" "29" "47"])]



(def sample-db (first (split-to-rules-and-updates sample)))
(def sample-updates (second (split-to-rules-and-updates sample)))

(def input-db (first (split-to-rules-and-updates input)))
(def input-updates (second (split-to-rules-and-updates input)))


(defn- validate-entry [db entry]
  #(not (contains? (db entry) %)))

(defn update-entry-valid?
  "Check if an update entry is valid"
  [db entry rest-entries]
  (every? (validate-entry db entry) rest-entries))

(update-entry-valid? sample-db "75" ["47" "61" "53" "29"])
;;=> true
(update-entry-valid? sample-db "47" ["53" "29"])
;;=> true
(update-entry-valid? sample-db "75" ["97" "47" "61" "53"])
;;=> false



(defn update-valid? [db u]
  (cond (empty? u) true
        (update-entry-valid? db (first u) (rest u)) (recur db (rest u))
        :else false))

(defn middle-entry [u]
  (read-string (nth u (quot (count u) 2))))

(middle-entry ["75" "47" "61" "53" "29"])
;;=> 61
(middle-entry  ["75" "29" "13"])
;;=> 29

(defn part1 [db u]
  (reduce + (map middle-entry (filter #(update-valid? db %) u))))

(part1 sample-db sample-updates)
;;=> 143

(part1 input-db input-updates)
;;=> 5639


(def bad-samples (filter #(not (update-valid? sample-db %)) sample-updates))
(def bad-input (filter #(not (update-valid? input-db %)) input-updates))





(defn maybe-fix-entry
  "pull the first entry from the rest-entries and then find all the 
   entries after that that have rule violations. For every entry 
   moved to the fixed entries list- also verify that there are no entries 
   related to that one that also need to be moved"
  [db fixed-entries rest-entries]
  (if (empty? rest-entries)
    fixed-entries
    (let [entry (first rest-entries)
          rest-entries (rest rest-entries)
          [good-entries bad-entries] (split-with (validate-entry db entry) rest-entries)]
      (if (empty? bad-entries)
        (maybe-fix-entry db (conj fixed-entries entry) rest-entries)
        (maybe-fix-entry db fixed-entries (concat bad-entries [entry] good-entries))))))

(maybe-fix-entry sample-db [] ["75" "47" "61" "53" "29"])
;;=> ["75" "47" "61" "53" "29"]
(maybe-fix-entry sample-db [] ["61" "13" "29"])



(defn part2 [db u]
  (reduce + (map middle-entry (map #(maybe-fix-entry db [] %) u))))


(part2 sample-db bad-samples)
;;=> 123
(part2 input-db bad-input)
;;=> 5273
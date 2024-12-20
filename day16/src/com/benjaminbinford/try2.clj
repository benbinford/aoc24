(ns com.benjaminbinford.try2
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map-by]])
  (:gen-class))





(defn parse [file]
  (str/split-lines (slurp file)))

(def input (parse "resources/input.txt"))
(def sample (parse "resources/sample.txt"))
(def sample2 (parse "resources/sample2.txt"))
(def sample3 (parse "resources/sample3.txt"))



(defn make-vertex [j i d]
  {:j j :i i :d d})


(defn graph-meta [input]
  (let [h (count input)
        w (count (first input))]
    {:h h :w w :start (make-vertex (- h 2) 1 :h) :ends [(make-vertex 1 (- w 2) :v) (make-vertex 1 (- w 2) :h)]}))

(graph-meta input)
;;=> {:h 141,
;;    :w 141,
;;    :starts [{:j 139, :i 1, :d :h} {:j 139, :i 1, :d :v}],
;;    :ends [{:j 1, :i 139, :d :v} {:j 1, :i 139, :d :h}]};;=> {:h 141, :w 141, :start {:j 139, :i 1, :d :h}, :ends [{:j 1, :i 139, :d :v} {:j 1, :i 139, :d :h}]}


(defn valid-cell? [input j i]
  (#{\. \S \E} (nth (nth input j) i)))

(defn has-neighbor? [input j i d]
  (case d
    :v (or (valid-cell? input (inc j) i) (valid-cell? input (dec j) i))
    :h (or (valid-cell? input j (inc i)) (valid-cell? input j (dec i)))))

(defn combine [m [k v]]
  (update m k (fnil conj []) v))


(defn neighbor? [v1 v2]

  (or (and (= (:j v1) (inc (:j v2))) (= (:i v1) (:i v2)))
      (and (= (:j v1) (dec (:j v2))) (= (:i v1) (:i v2)))
      (and (= (:j v1) (:j v2)) (= (:i v1) (inc (:i v2))))
      (and (= (:j v1) (:j v2)) (= (:i v1) (dec (:i v2))))))

(defn make-graph [input]
  (let [g (graph-meta input)
        vertices (vec (for [j (range (:h g)) i (range (:w g)) d [:v :h]
                            :when (valid-cell? input j i)
                            :let [v1 (make-vertex j i d)]
                            :when (or (= (:start g) v1) (has-neighbor? input j i d))]
                        (make-vertex j i d)))
        edges (reduce combine {} (for [v1 vertices v2 vertices
                                       :when (neighbor? v1 v2)]

                                   [v1 [v2 (if (= (:d v1) (:d v2)) 1 1001)]]))]
    (into g {:v vertices :e edges})))


(defn dist-map [g]
  (reduce (fn [m v] (assoc m v (if (= v (:start g)) 0 Integer/MAX_VALUE))) {} (:v g)))


(defn q-map [g]
  (priority-map-by <  (:start g) 0))

(q-map (make-graph sample3))

(defn dijkstra [g]
  (loop [{:keys [dist q] :as m} {:dist (dist-map g) :prev {} :q (q-map g)}]

    (if (empty? q)
      m
      (let [[u uw] (first q)
            m (update m :q dissoc u)
            neighbors (get-in g [:e u])]
        ;;(println "u  " u  "n  " neighbors)
        (recur
         (reduce (fn [m [v w]]
                   (let [alt (+ uw w)]
                   ;;  (println "v  " v "w  " w "alt  " alt  " new low " (< alt (get dist v)))
                     (if (<= alt (get dist v))
                       (-> m
                           (assoc-in [:dist v] alt)
                           (update-in [:prev v] (fnil conj []) u)
                           (update :q dissoc v)
                           (assoc-in [:q v] alt))
                       m)))
                 m neighbors))))))



(defn day1 [input]
  (let [g (make-graph input)
        {:keys [dist]} (dijkstra g)]
    (apply min (map (fn [v] (get dist v Integer/MAX_VALUE))
                    (:ends g)))))

(defn strip-direction [vs]
  (map #(dissoc % :d) vs))

(defn collect-priors [prevs vs]
  (loop [q (apply hash-set vs)
         seen #{}]
    (if (empty? q)
      seen
      (let [v (first q)
            q (rest q)]
     ;   (println q v seen (get prevs v))
        (cond
          (seen v) (recur q seen)
          :else (let [back (get prevs v)
                      seen (conj seen v)]
                 ; (println "back" back "seen" seen)
                  (recur (into q back)
                         seen)))))))
;;=> #'com.benjaminbinford.try2/collect-priors



(defn day2 [input]
  (day2 (make-graph input)))


(defn day2g [g]
  (let [{:keys [dist prev] :as m} (dijkstra g)
        target (apply min (map (fn [v] (get dist v Integer/MAX_VALUE))
                               (:ends g)))
        s (filter (fn [v] (= target (get dist v Integer/MAX_VALUE)))
                  (:ends g))]
   ;; (println (count (get m :prev)) (count prev))
    (count (into #{} (strip-direction (collect-priors prev s))))))

;;(day2 sample3)
;;=> 2006

;;(day2 sample2)
;;=> 7036


;;(day2 input)
;;=> 2147483647
;;=> 2006
;;=> [{:j 1, :i 13, :d :v} {:j 1, :i 13, :d :h}]


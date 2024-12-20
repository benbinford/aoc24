(ns com.benjaminbinford.day20
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map-by]])
  (:gen-class))





(defn parse [file]
  (str/split-lines (slurp file)))

(def input (parse "resources/input.txt"))
(def sample (parse "resources/sample.txt"))



(defn make-vertex [j i]
  {:j j :i i})



(defn find-in-grid [grid c]
  (some (fn [[j row]]
          (some (fn [[i kind]]
                  (when (= kind c) (make-vertex j i)))
                (map-indexed (fn [idx itm] [idx itm]) row)))
        (map-indexed (fn [idx itm] [idx itm]) grid)))

(find-in-grid sample \S)
;;=> {:j 3, :i 1}
(find-in-grid sample \E)
;;=> {:j 7, :i 5}


(defn graph-meta [input]
  (let [h (count input)
        w (count (first input))]
    {:h h :w w :start (find-in-grid input \S) :end (find-in-grid input \E)}))


(graph-meta sample)
;;=> {:h 15, :w 15, :start {:j 3, :i 1}, :end {:j 7, :i 5}}



(graph-meta input)
;;=> {:h 141, :w 141, :start {:j 69, :i 115}, :end {:j 79, :i 97}}


(defn valid-cell? [input h w v]
  (let [j (:j v)
        i (:i v)]
    (and (<= 0 j (dec h))
         (<= 0 i (dec w))
         (#{\. \S \E} (nth (nth input j) i)))))

(valid-cell? sample 15 15 (make-vertex 0 0))
;;=> nil
(valid-cell? sample 15 15 (make-vertex 3 1))
;;=> \S

(defn combine [m [k v]]
  (update m k (fnil conj []) v))


(defn neighbors [input h w v1]
  (let [j (:j v1)
        i (:i v1)]
    (when (valid-cell? input h w v1)
      (filter (partial valid-cell? input h w) [(make-vertex (inc j) i)
                                               (make-vertex (dec j) i)
                                               (make-vertex j (inc i))
                                               (make-vertex j (dec i))]))))


(neighbors sample 15 15 {:j 3 :i 1})
;;=> ({:j 2, :i 1})

(defn make-graph [input]
  (let [g (graph-meta input)
        h (:h g)
        w (:w g)
        vertices (apply hash-set (for [j (range (:h g)) i (range (:w g))
                                       :let [v1 (make-vertex j i)]
                                       :when (valid-cell? input h w v1)]
                                   v1))
        edges (reduce combine {} (for [v1 vertices v2 (neighbors input h w v1)]
                                   [v1 [v2 1]]))]
    (into g {:v vertices :e edges})))


(defn dist-map [g]
  (reduce (fn [m v] (assoc m v (if (= v (:start g)) 0 Integer/MAX_VALUE))) {} (:v g)))


(defn q-map [g]
  (priority-map-by <  (:start g) 0))

(q-map (make-graph sample))
;;=> {{:j 3, :i 1} 0}

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



(defn skip-neighbors [g v]

  (let [deltas [[1 0] [-1 0] [0 1] [0 -1]]]
    (for [[dj di] deltas
          :let [j (+ dj (:j v))
                i (+ di (:i v))
                j2 (+ dj j)
                i2 (+ di i)]
          :when (and
                 (not (contains? (:v g) (make-vertex j i)))
                 (contains? (:v g) (make-vertex j2 i2)))]
      (make-vertex j2 i2))))


(skip-neighbors (make-graph sample) {:j 1 :i 7})
;;=> ({:j 1, :i 9})

(defn cost-reduction [dist v1 skip-neighbor]
  (let [c1 (get dist v1)
        c2 (get dist skip-neighbor)]
   ;; (println dist "c1 " c1 " c2 " c2)
    (if (< c1 (+ 2 c2))
      (+ 2 (- c1 c2))
      nil)))

(def gs (make-graph sample))
(def ds (dijkstra gs))

(cost-reduction (:dist ds) {:j 1 :i 7} {:j 1 :i 9})
;;=> -12

(cost-reduction (:dist ds) {:j 7 :i 9} {:j 7 :i 11})
;;=> -20

(cost-reduction (:dist ds) {:j 7 :i 8} {:j 9 :i 8})
;;=> -38
(cost-reduction (:dist ds) {:j 7 :i 7} {:j 7 :i 5})
;;=> -64


(defn cheat-updater [cheats [v u c]]
  ;;(println "found cheat " c)
  (let [cheats (update cheats c (fnil inc 0))]
  ;;  (println "  cheats " cheats)
    cheats))

(defn find-cheats [g {:keys [dist prev]}]
  (loop [v (:end g)
         cheats {}]
   ;; (println v)
    (if (nil? v)
      cheats
      (recur (first (get prev v)) ;; our graphs only have a single line through
             (reduce cheat-updater cheats
                     (for [u (skip-neighbors g v)
                           :let [c (cost-reduction dist v u)]
                           :when c]
                       [v u c]))))))

(find-cheats gs ds)
;;=> {-12 3, -4 14, -64 1, -20 1, -2 14, -8 4, -6 2, -38 1, -36 1, -10 2, -40 1}
;;=> Execution error (NullPointerException) at com.benjaminbinford.day20/skip-neighbors$iter$fn$fn (REPL:108).
;;   Cannot invoke "Object.getClass()" because "x" is null
;;   

(defn sum-cheats [cheats min]
  (reduce + (for [[k v] cheats
                  :when (<= k min)]
              v)))

(defn day1 [input min]
  (let [g (make-graph input)
        d (dijkstra g)]
    (sum-cheats (find-cheats g d) min)))

(day1 sample -12)
;;=> 0

(day1 input -100)
;;=> 1197

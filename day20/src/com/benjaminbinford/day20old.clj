(ns com.benjaminbinford.day20old
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map-by]])
  (:gen-class))





(defn parse [file]
  (str/split-lines (slurp file)))

(def input (parse "resources/input.txt"))
(def sample (parse "resources/sample.txt"))
(def sample3 (parse "resources/sample3.txt"))



(defn make-vertex [j i value]
  {:j j :i i :value (case value \# \# \S \. \E \. \.)})




(defn find-in-grid [grid c]
  (some (fn [[j row]]
          (some (fn [[i kind]]
                  (when (= kind c) (make-vertex j i c)))
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



(defn any-cell? [h w v]
  (let [j (:j v)
        i (:i v)]
    (and (<= 0 j (dec h))
         (<= 0 i (dec w)))))

(defn valid-cell? [h w v]
  (let [j (:j v)
        i (:i v)]
    (and (<= 0 j (dec h))
         (<= 0 i (dec w))
         (#{\. \S \E} (:value v)))))

(valid-cell? 15 15 (make-vertex 0 0 \#))
;;=> nil
(valid-cell? 15 15 (make-vertex 3 1 \S))
;;=> \S

(defn combine [m [k v]]
  (update m k (fnil conj []) v))


(defn neighbors [input h w v1]
  (let [j (:j v1)
        i (:i v1)]
    (when (valid-cell? h w v1)
      (filter (partial valid-cell? h w) [(make-vertex (inc j) i (get (get input (inc j)) i))
                                         (make-vertex (dec j) i (get (get input (dec j)) i))
                                         (make-vertex j (inc i) (get (get input j) (inc i)))
                                         (make-vertex j (dec i) (get (get input j) (dec i)))]))))



(defn wall-neighbors [input h w v1]
  (let [j (:j v1)
        i (:i v1)]

    (filter #(any-cell? h w %)
            [(make-vertex (inc j) i (get (get input (inc j)) i))
             (make-vertex (dec j) i (get (get input (dec j)) i))
             (make-vertex j (inc i) (get (get input j) (inc i)))
             (make-vertex j (dec i) (get (get input j) (dec i)))])))

(neighbors sample 15 15 (make-vertex  3  1 \S))
;;=> ({:j 2, :i 1, :value \.})


(defn make-wall-graph [input]
  (let [g (graph-meta input)
        h (:h g)
        w (:w g)
        vertices (apply hash-set (for [j (range (:h g)) i (range (:w g))
                                       :let [v1 (make-vertex j i (nth (nth input j) i))]]
                                   v1))

        edges (reduce combine {} (for [v1 vertices
                                       :when (= \# (:value v1))
                                       v2 (wall-neighbors input h w v1)]
                                   [v1 [v2 1]]))]
    (into g {:v vertices :e edges})))

(defn make-graph [input]
  (let [g (graph-meta input)
        h (:h g)
        w (:w g)
        vertices (apply hash-set (for [j (range (:h g)) i (range (:w g))
                                       :let [v1 (make-vertex j i (nth (nth input j) i))]
                                       :when (valid-cell?  h w v1)]
                                   v1))




        edges (reduce combine {} (for [v1 vertices v2 (neighbors input h w v1)]
                                   [v1 [v2 1]]))]
    (into g {:v vertices :e edges})))


(defn dist-map [g]
  {(:start g) 0})


(defn q-map [g]
  (priority-map-by <  (:start g) 0))

(q-map (make-graph sample))
;;=> {{:j 3, :i 1} 0}

(defn dijkstra

  ([g] (dijkstra g Integer/MAX_VALUE))
  ([g cutoff]
   (loop [{:keys [dist q] :as m} {:dist (dist-map g) :prev {} :q (q-map g)}]

     (if (empty? q)
       m
       (let [[u uw] (first q)
             m (update m :q dissoc u)
             neighbors (get-in g [:e u])]
          ;;(println "u  " u  "n  " neighbors)
         (if (> uw cutoff)
           m
           (recur
            (reduce (fn [m [v w]]
                      (let [alt (+ uw w)]
                     ;;  (println "v  " v "w  " w "alt  " alt  " new low " (< alt (get dist v)))
                        (if (<= alt (get dist v Integer/MAX_VALUE))
                          (-> m
                              (assoc-in [:dist v] alt)
                              (update-in [:prev v] (fnil conj []) u)
                              (update :q dissoc v)
                              (assoc-in [:q v] alt))
                          m)))
                    m neighbors))))))))



(defn skip-neighbors [g v]

  (let [deltas [[1 0] [-1 0] [0 1] [0 -1]]]
    (for [[dj di] deltas
          :let [j (+ dj (:j v))
                i (+ di (:i v))
                j2 (+ dj j)
                i2 (+ di i)
                v2 (some #(get-in g [:v (make-vertex j2 i2 %)]) [\. \S \E])]

          :when (and
                 v2
                 (not (some #(contains? (:v g) (make-vertex j i %)) [\. \S \E])))]
      v2)))


(skip-neighbors (make-graph sample) {:j 1 :i 7})
;;=> ({:j 1, :i 9})

(defn cost-reduction [dist v1 skip-neighbor tunnel-distance]
  (let [c1 (get dist v1)
        c2 (get dist skip-neighbor)]
 ;;   (println  tunnel-distance "c1 " v1 c1 " c2 " skip-neighbor c2 " =  " (if (< (+ tunnel-distance c1) c2)
  ;;                                                                         (+ tunnel-distance (- c1 c2))
   ;;                                                                        nil) "\n")
    (if (< (+ tunnel-distance c1) c2)
      (+ tunnel-distance (- c1 c2))
      nil)))

(def gs (make-graph sample))
(def gws (make-wall-graph sample))
(def ds (dijkstra gs))

(cost-reduction (:dist ds) (make-vertex 1 7 \.) (make-vertex 1  9 \.) 2)
;;=> -12

(cost-reduction (:dist ds) (make-vertex 7 9 \.) (make-vertex 7  11 \.) 2)
;;=> -20

(cost-reduction (:dist ds) (make-vertex 7 8 \.) (make-vertex  9 8 \.) 2)
;;=> -38
(cost-reduction (:dist ds) (make-vertex 7 7 \.) (make-vertex  7 5 \E) 2)
;;=> -64



(defn add-tunnel [wall-graph {:keys [i j] :as start}]
  (let [walls (filter #(get-in wall-graph [:v %])
                      [(make-vertex (inc j) i \#)
                       (make-vertex (dec j) i \#)
                       (make-vertex j (inc i) \#)
                       (make-vertex j (dec i) \#)])]

    (-> wall-graph
        (assoc :start start)
        (update :e
                #(reduce combine %
                         (for [v2 walls]
                           [start [v2 1]]))))))

(defn tunnel-neighbors [wall-graph start course-dists max-tunnel-length]
  (let [wall-graph (add-tunnel wall-graph start)
        d (dijkstra wall-graph (inc max-tunnel-length))
        tunnel-ends (filter
                     #(and
                       (<= (dec (second %)) max-tunnel-length)
                       (= \. (:value (first %))))
                     (:dist d))]

    (doall (keep
            (fn [[v tunnel-cost]]
      ;;  (println "here" course-dists start v tunnel-cost)
              (let [reduction (cost-reduction course-dists start v  tunnel-cost)] ;; v and start are in the oposite directions because we are travelling forward through the walls as opposed to backwards in day1
                (when reduction
                  reduction)))
            tunnel-ends))))




(defn cheat-updater [cheats c]
 ;; (println "found cheat " c)
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
                           :let [c (cost-reduction dist v u 2)]
                           :when c]
                       c))))))


(defn find-tunnels [g {:keys [dist prev]} wall-graph max-tunnel-length]
  (loop [v (:end g)
         cheats {}]

    (if (nil? v)
      (do
        (println cheats)
        cheats)
      (recur (first (get prev v)) ;; our graphs only have a single line through
             (reduce cheat-updater cheats
                     (tunnel-neighbors wall-graph v dist max-tunnel-length))))))


(find-cheats gs ds)
;;=> {-12 3, -4 14, -64 1, -20 1, -2 14, -8 4, -6 2, -38 1, -36 1, -10 2, -40 1}


(defn sum-cheats [cheats min]
  (reduce + (for [[k v] cheats
                  :when (<= k min)]
              v)))

(defn day1 [input min]
  (let [g (make-graph input)
        d (dijkstra g)]
    (sum-cheats (find-cheats g d) min)))

(day1 sample -12)
;;=> 8
;;=> 8

(day1 input -100)
;;=> 1197



(defn day2 [input min max-tunnel-length]
  (let [g (make-graph input)
        d (dijkstra g)
        wall-graph (make-wall-graph input)]
    (sort-by first (find-tunnels g d wall-graph max-tunnel-length))))

(day2 sample -50 20) d
;;=> ([-76 3]
;;    [-74 4]
;;    [-72 12]
;;    [-70 10]
;;    [-68 8]
;;    [-66 11]
;;    [-64 19]
;;    [-62 18]
;;    [-60 19]
;;    [-58 18]
;;    [-56 21]
;;    [-54 22]
;;    [-52 27]
;;    [-50 36]
;;    [-48 45]
;;    [-46 37]
;;    [-44 62]
;;    [-42 50]
;;    [-40 37]
;;    [-38 49]
;;    [-36 47]
;;    [-34 43]
;;    [-32 45]
;;    [-30 47]
;;    [-28 55]
;;    [-26 52]
;;    [-24 62]
;;    [-22 66]
;;    [-20 73]
;;    [-18 82]
;;    [-16 97]
;;    [-14 109]
;;    [-12 129]
;;    [-10 149]
;;    [-8 168]
;;    [-6 164]
;;    [-4 206]
;;    [-2 258])
;;=> 44
(day2 input -99  20)
;;=> 238953

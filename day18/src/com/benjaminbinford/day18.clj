(ns com.benjaminbinford.day18
  (:require [clojure.spec.alpha :as s])
  (:require [clojure.string :as str])
  (:gen-class)
  (:require [clojure.data.priority-map :refer [priority-map-by]]))

(defn parse [file]
  (mapv (fn [line]
          (mapv Integer/parseInt (str/split line #",")))
        (str/split-lines (slurp file))))

(def input (parse "resources/input.txt"))
(def sample (parse "resources/sample.txt"))

sample
;;=> [[5 4]
;;    [4 2]
;;    [4 5]
;;    [3 0]
;;    [2 1]
;;    [6 3]
;;    [2 4]
;;    [1 5]
;;    [0 6]
;;    [3 3]
;;    [2 6]
;;    [5 1]
;;    [1 2]
;;    [5 5]
;;    [2 5]
;;    [6 5]
;;    [1 4]
;;    [0 4]
;;    [6 4]
;;    [1 1]
;;    [6 1]
;;    [1 0]
;;    [0 5]
;;    [1 6]
;;    [2 0]]

(defn ->blocks [input n]
  (into #{} (take n input)))

(->blocks sample 12)
;;=> #{[0 6] [3 3] [5 4] [6 3] [4 2] [3 0] [1 5] [5 1] [2 4] [4 5] [2 1] [2 6]}

(defn display [blocks h w]
  (let [empty-buffer (transient (vec (repeat (* h w) \.)))
        index (fn [[i j]] (+ (* w j) i))
        buffer (persistent! (reduce
                             (fn [buffer [i j]]
                               (assoc! buffer (index [i j]) \#))
                             empty-buffer blocks))]
    (doseq [line (partition w buffer)]
      (println (apply str line)))))

(display (->blocks sample 12) 7 7)


(defn create-q [start]
  (into (priority-map-by <) [start]))

(create-q [[0 0] 0])
;;=> {[0 0] 0}

(defn lowest-cost-q [state]
  (let [loc (first (get state :q))
        state (update state :q dissoc (first loc))]
    [loc state]))


(defn add-q [state loc cost]
  (update state :q assoc loc cost))


(defn initial-state [blocks h w]
  {:blocks blocks
   :h h
   :w w
   :q (create-q [[0 0] 0])
   :d {[0 0] 0}})

(lowest-cost-q (initial-state (->blocks sample 12) 7 7))
;;=> [[[0 0] 0] {:blocks #{[0 6] [3 3] [5 4] [6 3] [4 2] [3 0] [1 5] [5 1] [2 4] [4 5] [2 1] [2 6]}, :h 7, :w 7, :q {}}]





(defn available-exits [loc state]


  (keep (fn [exit]
          (when (and
                 (>= (first exit) 0)
                 (>= (second exit) 0)
                 (< (first exit) (get-in state [:w]))
                 (< (second exit) (get-in state [:h]))
                 (not ((:blocks state) exit))
                 (not (get-in state [:d exit])))
            exit))
        (map (fn [exit-direction]
               (mapv + loc exit-direction))
             [[0 1] [0 -1] [1 0] [-1 0]])))

(available-exits [0 0] (initial-state (->blocks sample 12) 7 7))
;;=> ([0 1] [1 0])



(defn update-state-for-exit [state [loc cost]]
  (if (< cost (get-in state [:d loc] Integer/MAX_VALUE))
    (add-q state loc cost)
    state))

(defn dijkstra-step [state]
  (s/assert ::state state)
  (s/assert
   ::state
   (let [[[loc cost] state] (lowest-cost-q state)
         state (update state :d assoc loc cost)
         exits (mapv #(vector % (+ cost 1)) (available-exits loc state))]
     ;;(println " cur" cur "\n\n" " exits" exits "\n\n q" (::q state))
     (reduce update-state-for-exit
             state
             exits))))


(dijkstra-step (initial-state (->blocks sample 12) 7 7))
;;=> {:blocks #{[0 6] [3 3] [5 4] [6 3] [4 2] [3 0] [1 5] [5 1] [2 4] [4 5] [2 1] [2 6]},
;;    :h 7,
;;    :w 7,
;;    :q {[1 0] 1, [0 1] 1},
;;    :d {[0 0] 0}}

(dijkstra-step (dijkstra-step (initial-state (->blocks sample 12) 7 7)))
;;=> {:blocks #{[0 6] [3 3] [5 4] [6 3] [4 2] [3 0] [1 5] [5 1] [2 4] [4 5] [2 1] [2 6]},
;;    :h 7,
;;    :w 7,
;;    :q {[0 1] 1, [1 1] 2, [2 0] 2},
;;    :d {[0 0] 0, [1 0] 1}}

(dijkstra-step (dijkstra-step (dijkstra-step (initial-state (->blocks sample 12) 7 7))))
;;=> {:blocks #{[0 6] [3 3] [5 4] [6 3] [4 2] [3 0] [1 5] [5 1] [2 4] [4 5] [2 1] [2 6]},
;;    :h 7,
;;    :w 7,
;;    :q {[1 1] 2, [0 2] 2, [2 0] 2},
;;    :d {[0 0] 0, [1 0] 1, [0 1] 1}}

(defn dijkstra [state]
  (loop [state state
         acc 0]
    (if (or (> acc 10000) (empty? (get state :q)))
      state
      (recur (dijkstra-step state) (inc acc)))))

(dijkstra (initial-state (->blocks sample 12) 7 7))
;;=> {:blocks #{[0 6] [3 3] [5 4] [6 3] [4 2] [3 0] [1 5] [5 1] [2 4] [4 5] [2 1] [2 6]},
;;    :h 7,
;;    :w 7,
;;    :q {},
;;    :d
;;    {[4 3] 15,
;;     [2 2] 4,
;;     [0 0] 0,
;;     [1 0] 1,
;;     [2 3] 5,
;;     [2 5] 19,
;;     [1 1] 2,
;;     [0 5] 5,
;;     [3 4] 17,
;;     [6 6] 22,
;;     [5 3] 14,
;;     [6 5] 23,
;;     [4 1] 7,
;;     [5 2] 13,
;;     [4 6] 20,
;;     [1 4] 5,
;;     [1 3] 4,
;;     [6 4] 24,
;;     [0 3] 3,
;;     [6 1] 11,
;;     [5 6] 21,
;;     [5 5] 22,
;;     [3 6] 19,
;;     [0 2] 2,
;;     [2 0] 2,
;;     [0 4] 4,
;;     [3 1] 6,
;;     [4 4] 16,
;;     [5 0] 9,
;;     [6 2] 12,
;;     [6 0] 10,
;;     [1 2] 3,
;;     [3 5] 18,
;;     [3 2] 5,
;;     [0 1] 1,
;;     [4 0] 8}}


(defn blocked? [n blocks h w]
  (when (nil? (get-in (dijkstra (initial-state (->blocks blocks n)  h w)) [:d [(dec w) (dec h)]]))
    (nth blocks (dec n))))

(dijkstra (initial-state (->blocks sample 12)  7 7))
;;=> {:blocks #{[0 6] [3 3] [5 4] [6 3] [4 2] [3 0] [1 5] [5 1] [2 4] [4 5] [2 1] [2 6]},
;;    :h 7,
;;    :w 7,
;;    :q {},
;;    :d
;;    {[4 3] 15,
;;     [2 2] 4,
;;     [0 0] 0,
;;     [1 0] 1,
;;     [2 3] 5,
;;     [2 5] 19,
;;     [1 1] 2,
;;     [0 5] 5,
;;     [3 4] 17,
;;     [6 6] 22,
;;     [5 3] 14,
;;     [6 5] 23,
;;     [4 1] 7,
;;     [5 2] 13,
;;     [4 6] 20,
;;     [1 4] 5,
;;     [1 3] 4,
;;     [6 4] 24,
;;     [0 3] 3,
;;     [6 1] 11,
;;     [5 6] 21,
;;     [5 5] 22,
;;     [3 6] 19,
;;     [0 2] 2,
;;     [2 0] 2,
;;     [0 4] 4,
;;     [3 1] 6,
;;     [4 4] 16,
;;     [5 0] 9,
;;     [6 2] 12,
;;     [6 0] 10,
;;     [1 2] 3,
;;     [3 5] 18,
;;     [3 2] 5,
;;     [0 1] 1,
;;     [4 0] 8}}

(get-in (dijkstra (initial-state (->blocks input 1024) 71 71)) [:d [70 70]])
;;=> 262

(blocked? 21 sample 7 7)
;;=> [6 1]

(blocked? 3448)
;;=> [46 2]
(blocked? 1024)

(blocked? 2974 input 71 71)
;;=> [22 20]

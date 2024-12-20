(ns com.benjaminbinford.day16
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map-by]])
  (:gen-class))

(s/check-asserts false)

(s/def ::kind (s/nilable #{\. \# \S \E}))
(s/def ::row (s/coll-of ::kind :kind vector?))
(s/def ::grid (s/coll-of ::row :kind vector?))
(s/def ::direction #{::n ::s ::w ::e})
(s/def ::loc (s/tuple int? int? ::direction))
(s/def ::cost number?)
(s/def ::visit (s/tuple ::loc ::cost))
(s/def ::q (s/map-of ::loc ::cost))
(s/def ::distances (s/map-of ::loc number?))
(s/def ::priors (s/map-of ::loc (s/coll-of ::loc)))
(s/def ::seen (s/coll-of ::loc :kind set?))
(s/def ::state (s/keys :req [::grid ::distances ::priors ::q]))

(defn parse [file]
  (s/assert ::grid (mapv #(reduce conj [] %) (str/split-lines (slurp file)))))
()
(def input (parse "resources/input.txt"))
(def sample (parse "resources/sample.txt"))
(def sample2 (parse "resources/sample2.txt"))
(def sample3 (parse "resources/sample3.txt"))


(defn make-visit [loc cost]
  (s/assert ::loc loc)
  (s/assert ::cost cost)
  [loc cost])


(defn find-start [grid]
  (s/assert ::grid grid)
  (s/assert ::visit (make-visit (some (fn [[j row]]
                                        (some (fn [[i kind]]
                                                (when (= kind \S) [j i ::e]))
                                              (map-indexed (fn [idx itm] [idx itm]) row)))
                                      (map-indexed (fn [idx itm] [idx itm]) grid))
                                0)))

(find-start sample)
;;=> [[13 1 :com.benjaminbinford.day16/e] 0]


(defn find-ends [grid]
  (s/assert ::grid grid)
  (s/assert (s/coll-of ::loc)
            (let [coords (some (fn [[j row]]
                                 (some (fn [[i kind]]
                                         (when (= kind \E) [j i]))
                                       (map-indexed (fn [idx itm] [idx itm]) row)))
                               (map-indexed (fn [idx itm] [idx itm]) grid))]
              (into #{} (map #(conj coords %) [::n ::s ::w ::e])))))

(find-ends sample)
;;=> #{[1 13 :com.benjaminbinford.day16/s]
;;     [1 13 :com.benjaminbinford.day16/w]
;;     [1 13 :com.benjaminbinford.day16/n]
;;     [1 13 :com.benjaminbinford.day16/e]}

(defn move-cost [start-direction end-direction]
  (s/assert ::direction start-direction)
  (s/assert ::direction end-direction)
  (s/assert number?
            (case start-direction
              (::n ::s) (case end-direction
                          (::n ::s) 1
                          (::w ::e) 1001)
              (::w ::e) (case end-direction
                          (::w ::e) 1
                          (::n ::s) 1001))))


(defn loc-coords [loc]
  (s/assert ::loc loc)
  (take 2 loc))

(loc-coords [1 1 ::e])
;;=> (1 1)

(defn loc-direction [loc]
  (s/assert ::loc loc)
  (last loc))

(loc-direction [1 1 ::e])
;;=> :com.benjaminbinford.day16/e

(defn visit-loc [visit]
  (s/assert ::visit visit)
  (first visit))

(visit-loc [[1 1 ::e] 1])
;;=> [1 1 :com.benjaminbinford.day16/e]

(defn visit-cost [visit]
  (s/assert ::visit visit)
  (last visit))


(visit-cost [[1 1 ::e] 1])
;;=> 1
(defn create-q [start]
  (s/assert ::visit start)
  (s/assert ::q (into (priority-map-by <) [start])))

(defn lowest-cost-q [state]
  (s/assert ::state state)
  (let [visit (first (get state ::q))
        state (update state ::q dissoc (visit-loc visit))]
    [(s/assert ::visit visit) (s/assert ::state state)]))



(defn add-q [state loc cost]
  (update state ::q assoc loc cost))

(defn initial-state [grid]
  (s/assert ::grid grid)
  (s/assert ::state
            {::distances {(visit-loc (find-start grid)) 0}
             ::priors {}
             ::q (create-q (find-start grid))
             ::grid grid}))


(lowest-cost-q (initial-state sample))
;;=> [[[13 1 :com.benjaminbinford.day16/e] 0]
;;    #:com.benjaminbinford.day16{:distances {},
;;                                :priors {},
;;                                :seen #{},
;;                                :q {},
;;                                :grid
;;                                [[\# \# \# \# \# \# \# \# \# \# \# \# \# \# \#]
;;                                 [\# \. \. \. \. \. \. \. \# \. \. \. \. \E \#]
;;                                 [\# \. \# \. \# \# \# \. \# \. \# \# \# \. \#]
;;                                 [\# \. \. \. \. \. \# \. \# \. \. \. \# \. \#]
;;                                 [\# \. \# \# \# \. \# \# \# \# \# \. \# \. \#]
;;                                 [\# \. \# \. \# \. \. \. \. \. \. \. \# \. \#]
;;                                 [\# \. \# \. \# \# \# \# \# \. \# \# \# \. \#]
;;                                 [\# \. \. \. \. \. \. \. \. \. \. \. \# \. \#]
;;                                 [\# \# \# \. \# \. \# \# \# \# \# \. \# \. \#]
;;                                 [\# \. \. \. \# \. \. \. \. \. \# \. \# \. \#]
;;                                 [\# \. \# \. \# \. \# \# \# \. \# \. \# \. \#]
;;                                 [\# \. \. \. \. \. \# \. \. \. \# \. \# \. \#]
;;                                 [\# \. \# \# \# \. \# \. \# \. \# \. \# \. \#]
;;                                 [\# \S \. \. \# \. \. \. \. \. \# \. \. \. \#]
;;                                 [\# \# \# \# \# \# \# \# \# \# \# \# \# \# \#]]}]
;;(first ( ::q (initial-state sample)))
;;(disj (::q (initial-state sample)) (first (::q (initial-state sample))) )
(initial-state sample)
;;=> #:com.benjaminbinford.day16{:distances {},
;;                               :priors {},
;;                               :seen #{},
;;                               :q {[13 1 :com.benjaminbinford.day16/e] 0},
;;                               :grid
;;                               [[\# \# \# \# \# \# \# \# \# \# \# \# \# \# \#]
;;                                [\# \. \. \. \. \. \. \. \# \. \. \. \. \E \#]
;;                                [\# \. \# \. \# \# \# \. \# \. \# \# \# \. \#]
;;                                [\# \. \. \. \. \. \# \. \# \. \. \. \# \. \#]
;;                                [\# \. \# \# \# \. \# \# \# \# \# \. \# \. \#]
;;                                [\# \. \# \. \# \. \. \. \. \. \. \. \# \. \#]
;;                                [\# \. \# \. \# \# \# \# \# \. \# \# \# \. \#]
;;                                [\# \. \. \. \. \. \. \. \. \. \. \. \# \. \#]
;;                                [\# \# \# \. \# \. \# \# \# \# \# \. \# \. \#]
;;                                [\# \. \. \. \# \. \. \. \. \. \# \. \# \. \#]
;;                                [\# \. \# \. \# \. \# \# \# \. \# \. \# \. \#]
;;                                [\# \. \. \. \. \. \# \. \. \. \# \. \# \. \#]
;;                                [\# \. \# \# \# \. \# \. \# \. \# \. \# \. \#]
;;                                [\# \S \. \. \# \. \. \. \. \. \# \. \. \. \#]
;;                                [\# \# \# \# \# \# \# \# \# \# \# \# \# \# \#]]}

(defn move-loc [loc exit-direction]
  (s/assert ::loc loc)
  (s/assert ::direction exit-direction)
  (s/assert ::loc  (let [[y x _] loc]
                     (case exit-direction
                       ::n [(- y 1) x ::n]
                       ::s [(+ y 1) x ::s]
                       ::w [y (- x 1) ::w]
                       ::e [y (+ x 1) ::e]))))


(move-loc [1 1 ::e] ::n)

(defn not-backwards [[[_ _ d1] _] [[_ _ d2] _]]
  (case d1
    ::e (not= d2 ::w)
    ::w (not= d2 ::e)
    ::s (not= d2 ::n)
    ::n (not= d2 ::s)))

(not-backwards (make-visit [1 1 ::n] 10)  (make-visit [2 1 ::s] 10))
;;=> false
;;=> true
;;=> [0 1 :com.benjaminbinford.day16/n]


(defn available-exits [visit grid]
  (s/assert ::grid grid)
  (s/assert (s/coll-of ::visit)
            (let [loc (visit-loc visit)
                  cost (visit-cost visit)]
              (keep (fn [exit]
                      (when (and
                             (#{\. \S \E} (get-in grid (loc-coords (visit-loc exit))))
                             (not-backwards visit exit))
                        exit))
                    (map (fn [exit-direction]
                           (make-visit
                            (move-loc loc exit-direction)
                            (+ cost (move-cost (loc-direction loc) exit-direction))))
                         [::n ::e ::s ::w])))))

(available-exits (find-start sample)  (::grid (update-in (initial-state sample) [::seen] conj [12 1 ::n])))
;;=> ([[12 1 :com.benjaminbinford.day16/n] 1001] [[13 2 :com.benjaminbinford.day16/e] 1])


(defn update-state-for-exit [cur state exit]
  (s/assert ::state state)
  (s/assert ::visit exit)
  (s/assert ::visit cur)
  (let [loc (visit-loc exit)
        cost (visit-cost exit)]
    (cond
      (not (contains? (::seen state) loc))
      (-> state
          (update ::seen conj loc)
          (update ::distances assoc loc cost)
          (update ::priors assoc loc [(visit-loc cur)])
          (add-q loc cost))

      (= cost (get-in (::distances state) loc))
      (update-in state [::priors loc] conj (visit-loc cur))

      :else
      state)))

(defn dijkstra-step [state]
  (s/assert ::state state)
  (s/assert
   ::state
   (let [[cur state] (lowest-cost-q state)
         exits (available-exits cur (::grid state))]
     ;;(println " cur" cur "\n\n" " exits" exits "\n\n q" (::q state))
     (reduce (partial update-state-for-exit cur)
             state
             exits))))


(dijkstra-step (initial-state sample))
;;=> #:com.benjaminbinford.day16{:distances
;;                               {[12 1 :com.benjaminbinford.day16/n] 1001, [13 2 :com.benjaminbinford.day16/e] 1},
;;                               :priors
;;                               {[12 1 :com.benjaminbinford.day16/n] [13 1 :com.benjaminbinford.day16/e],
;;                                [13 2 :com.benjaminbinford.day16/e] [13 1 :com.benjaminbinford.day16/e]},
;;                               :seen #{[13 2 :com.benjaminbinford.day16/e] [12 1 :com.benjaminbinford.day16/n]},
;;                               :q {[12 1 :com.benjaminbinford.day16/n] 1001, [13 2 :com.benjaminbinford.day16/e] 1},
;;                               :grid
;;                               [[\# \# \# \# \# \# \# \# \# \# \# \# \# \# \#]
;;                                [\# \. \. \. \. \. \. \. \# \. \. \. \. \E \#]
;;                                [\# \. \# \. \# \# \# \. \# \. \# \# \# \. \#]
;;                                [\# \. \. \. \. \. \# \. \# \. \. \. \# \. \#]
;;                                [\# \. \# \# \# \. \# \# \# \# \# \. \# \. \#]
;;                                [\# \. \# \. \# \. \. \. \. \. \. \. \# \. \#]
;;                                [\# \. \# \. \# \# \# \# \# \. \# \# \# \. \#]
;;                                [\# \. \. \. \. \. \. \. \. \. \. \. \# \. \#]
;;                                [\# \# \# \. \# \. \# \# \# \# \# \. \# \. \#]
;;                                [\# \. \. \. \# \. \. \. \. \. \# \. \# \. \#]
;;                                [\# \. \# \. \# \. \# \# \# \. \# \. \# \. \#]
;;                                [\# \. \. \. \. \. \# \. \. \. \# \. \# \. \#]
;;                                [\# \. \# \# \# \. \# \. \# \. \# \. \# \. \#]
;;                                [\# \S \. \. \# \. \. \. \. \. \# \. \. \. \#]
;;                                [\# \# \# \# \# \# \# \# \# \# \# \# \# \# \#]]}


(defn dijkstra [state]
  (s/assert ::state state)
  (s/assert ::state
            (loop [state state]
             ;; (println state "\n\n")
              (if (empty? (get state ::q))
                state
                (recur (dijkstra-step state))))))

(defn day1 [input]
  (let [s (initial-state input)
        s (dijkstra s)
        ds (seq (::distances s))]
    (apply min (keep (fn [end] (second (first (filter (fn [v] (= (visit-loc v) end)) ds))))
                     (find-ends input)))))


(day1 sample)
;;=> 7036
;;=> (7036 nil nil 10028)
;;=> (([[1 13 :com.benjaminbinford.day16/n] 7036]) () () ([[1 13 :com.benjaminbinford.day16/e] 8038]))
;;=> (([[1 13 :com.benjaminbinford.day16/n] 7036]) () () ([[1 13 :com.benjaminbinford.day16/e] 8038]))

(day1 sample2)
;;=> (([[1 15 :com.benjaminbinford.day16/n] 11048]) () () ([[1 15 :com.benjaminbinford.day16/e] 12048]))

(day1 input)
;;=> 98520

;; (defn day2 [input]
;;   (s/assert ::grid input)
;;   (let [target (day1 input)
;;         start (find-start input)
;;         ends (find-ends input)]
;;     (search target ends start  )))


(ns com.benjaminbinford.day16
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map-by]])
  (:gen-class))

(s/check-asserts true)

(s/def ::kind (s/nilable #{\. \# \S \E}))
(s/def ::row (s/coll-of ::kind :kind vector?))
(s/def ::grid (s/coll-of ::row :kind vector?))
(s/def ::direction #{::n ::s ::w ::e})
(s/def ::loc (s/tuple int? int? ::direction))
(s/def ::cost number?)
(s/def ::visit (s/tuple ::loc ::cost))
(s/def ::q (s/map-of ::loc ::cost))
(s/def ::distances (s/map-of ::loc number?))
(s/def ::priors (s/map-of ::loc ::loc))
(s/def ::seen (s/coll-of ::loc :kind set?))
(s/def ::state (s/keys :req [::grid ::distances ::priors ::q]))

(defn parse [file]
  (s/assert ::grid (mapv #(reduce conj [] %) (str/split-lines (slurp file)))))
()
(def input (parse "resources/input.txt"))
(def sample (parse "resources/sample.txt"))
(def sample2 (parse "resources/sample2.txt"))


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
              (map #(conj coords %) [::n ::s ::w ::e]))))

(find-ends sample)
;;=> ([1 13 :com.benjaminbinford.day16/n]
;;    [1 13 :com.benjaminbinford.day16/s]
;;    [1 13 :com.benjaminbinford.day16/w]
;;    [1 13 :com.benjaminbinford.day16/e])

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

(defn visit-comparator [[[ai aj ad] costa]
                        [[bi bj bd] costb]]
  (or (some (fn [[x y]]

              (let [c (compare x y)]
                (when (not= c 0)
                  c)))
            [[costa costb] [ai bi] [aj bj] [ad bd]])
      0))


(visit-comparator [[1 1 ::n] 1] [[1 1 ::n] 2])
;;=> -1
(visit-comparator [[1 1 ::n] 13] [[1 1 ::n] 2])
;;=> 1
(visit-comparator [[1 1 ::n] 2] [[1 1 ::s] 2])
;;=> -5



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
            {::distances {}
             ::priors {}
             ::seen #{}
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
;;=> [0 1 :com.benjaminbinford.day16/n]


(defn available-exits [visit {:keys [::grid ::seen] :as state}]
  (s/assert ::state state)
  (s/assert (s/coll-of ::visit)
            (let [loc (visit-loc visit)
                  cost (visit-cost visit)]
              (keep (fn [exit]
                      (when (and (not (seen (visit-loc exit)))
                                 (#{\. \S \E} (get-in grid (loc-coords (visit-loc exit)))))
                        exit))
                    (map (fn [exit-direction]
                           (make-visit
                            (move-loc loc exit-direction)
                            (+ cost (move-cost (loc-direction loc) exit-direction))))
                         [::n ::e ::s ::w])))))

(available-exits (find-start sample)  (update-in (initial-state sample) [::seen] conj [12 1 ::n]))
;;=> ([[13 2 :com.benjaminbinford.day16/e] 1])

(available-exits (find-start sample)  (update-in (initial-state sample) [::seen] conj [13 2 ::e]))
;;=> ([[12 1 :com.benjaminbinford.day16/n] 1001])
(available-exits  (find-start sample) (initial-state sample))
;;=> ([[12 1 :com.benjaminbinford.day16/n] 1001] [[13 2 :com.benjaminbinford.day16/e] 1])


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
(lowest-cost-q (-> (initial-state sample)
                   (assoc-in [::q [13 1 :com.benjaminbinford.day16/e]] 28)
                   (assoc-in [::q  [14 1 :com.benjaminbinford.day16/e]] 27)))
;;=> [[[14 1 :com.benjaminbinford.day16/e] 27]
;;    #:com.benjaminbinford.day16{:distances {},
;;                                :priors {},
;;                                :seen #{},
;;                                :q {[13 1 :com.benjaminbinford.day16/e] 28},
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
;;=> [[[13 1 :com.benjaminbinford.day16/e] 24]
;;    #:com.benjaminbinford.day16{:distances {},
;;                                :priors {},
;;                                :seen #{},
;;                                :q {[14 1 :com.benjaminbinford.day16/e] 27},
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
;;=> ([[13 1 :com.benjaminbinford.day16/e] 24] [[14 1 :com.benjaminbinford.day16/e] 23])
;;=> [[[13 1 :com.benjaminbinford.day16/e] 24]
;;    #:com.benjaminbinford.day16{:distances {},
;;                                :priors {},
;;                                :seen #{},
;;                                :q {[14 1 :com.benjaminbinford.day16/e] 23},
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
;;=> [[[13 1 :com.benjaminbinford.day16/e] 20]
;;    #:com.benjaminbinford.day16{:distances {},
;;                                :priors {},
;;                                :seen #{},
;;                                :q {[14 1 :com.benjaminbinford.day16/e] 23},
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
;;=> [[[13 1 :com.benjaminbinford.day16/e] 20]
;;    #:com.benjaminbinford.day16{:distances {},
;;                                :priors {},
;;                                :seen #{},
;;                                :q {[14 1 :com.benjaminbinford.day16/e] 23},
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
;;=> [[[13 1 :com.benjaminbinford.day16/e] 20]
;;    #:com.benjaminbinford.day16{:distances {},
;;                                :priors {},
;;                                :seen #{},
;;                                :q {[14 1 :com.benjaminbinford.day16/e] 23},
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


(defn dijkstra-step [state]
  (s/assert ::state state)
  (s/assert
   ::state
   (let [[cur state] (lowest-cost-q state)
         exits (filter #(not ((::seen state) (visit-loc %))) (available-exits cur state))]
     (reduce (fn [state exit]
               (let [loc (visit-loc exit)
                     cost (visit-cost exit)]
                 (-> state
                     (update ::seen conj loc)
                     (update ::distances assoc loc cost)
                     (update ::priors assoc loc (visit-loc cur))
                     (add-q loc cost))))
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
              (when (zero? (rem (count (::seen state)) 100))
                (println (count (::seen state))))
              (if (empty? (get state ::q))
                state
                (recur (dijkstra-step state))))))

(defn day1 [input]
  (let [s (initial-state input)
        s (dijkstra s)
        ds (seq (::distances s))]
    (map (fn [end] (filter (fn [v] (= (visit-loc v) end)) ds))
         (find-ends input))))


(day1 sample)
;;=> (([[1 13 :com.benjaminbinford.day16/n] 7036]) () () ([[1 13 :com.benjaminbinford.day16/e] 8038]))

(day1 sample2)
;;=> (([[1 15 :com.benjaminbinford.day16/n] 11048]) () () ([[1 15 :com.benjaminbinford.day16/e] 12048]))

;;(day1 input)



























































































































































































































































































































































































































































































































































































































































































































































































































(ns com.benjaminbinford.day21
  (:require [clojure.string :as str])
  (:gen-class))


(def sample ["029A"
             "980A"
             "179A"
             "456A"
             "379A"])

(def sample-results
  {"029A" "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
   "980A" "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"
   "179A" "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
   "456A" "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A"
   "379A" "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"})

(def input ["319A"
            "670A"
            "349A"
            "964A"
            "586A"])




(def numpad
  {\7 [0 0], \8 [0 1], \9 [0 2],
   \4 [1 0], \5 [1 1], \6 [1 2],
   \1 [2 0], \2 [2 1], \3 [2 2],
   \0 [3 1], \A [3 2]})

(def dirpad
  {\^ [0 1], \A [0 2],
   \< [1 0], \v [1 1], \> [1 2]})

(defn create-graph [keypad invalid-coords]
  (reduce (fn [graph [a [x1 y1]]]
            (reduce (fn [graph [b [x2 y2]]]
                      (let [path (str (apply str (repeat (- y1 y2) \<))
                                      (apply str (repeat (- x2 x1) \v))
                                      (apply str (repeat (- x1 x2) \^))
                                      (apply str (repeat (- y2 y1) \>)))
                            path (if (or (= invalid-coords [x1 y2])
                                         (= invalid-coords [x2 y1]))
                                   (apply str (reverse path))
                                   path)]
                        (assoc graph [a b] (str path \A))))
                    graph
                    keypad))
          {}
          keypad))

(def numpad-graph (create-graph numpad [3 0]))
(def dirpad-graph (create-graph dirpad [0 0]))

(defn moves [input graph]
  (loop [result ""
         prev \A
         input input]
    (if (empty? input)
      result
      (let [c (first input)
            path (get graph [prev c])]
        (recur (str result path) c (rest input))))))

(defn part1 [input]
  (reduce (fn [total-complexity button-presses]
            (let [ms (-> button-presses
                         (moves numpad-graph)
                         (moves dirpad-graph)
                         (moves dirpad-graph))]
              (+ total-complexity (* (Integer/parseInt (subs button-presses 0 (dec (count button-presses))))
                                     (count ms)))))
          0
          input))



(part1 sample)
;;=> 126384

(part1 input)
;;=> 202274

(def moves-length

  (memoize (fn [moves-length sequence iterations first-iter]
             (if (zero? iterations)
               (count sequence)
               (let [graph (if first-iter numpad-graph dirpad-graph)]
                 (loop [total-length 0
                        prev \A
                        chars sequence]
                   (if (empty? chars)
                     total-length
                     (let [char (first chars)
                           path (get graph [prev char])]
                       (recur (+ total-length (moves-length moves-length path (dec iterations) false))
                              char
                              (rest chars))))))))))

(defn part2 [input]
  (reduce (fn [total-complexity button-presses]
            (+ total-complexity
               (* (Integer/parseInt (subs button-presses 0 3))
                  (moves-length moves-length button-presses 26 true))))
          0
          input))


(part2 sample)
;;=> 154115708116294

(part2 input)
;;=> 245881705840972
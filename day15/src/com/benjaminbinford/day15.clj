(ns com.benjaminbinford.day15
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s])
  (:gen-class))

(s/def ::w int?)
(s/def ::h int?)
(s/def ::kind (s/nilable #{::wall ::box ::robot}))
(s/def ::p (s/tuple int? int?))
(s/def ::raw-position (s/keys :req [::p ::kind]))
(s/def ::raw-positions (s/coll-of ::raw-position))
(s/def ::position-stream (s/keys :req [::w ::h ::raw-positions]))
(s/def ::positions (s/map-of ::p ::kind))
(s/def ::position-list (s/coll-of ::p))
(s/def ::robot ::p)
(s/def ::block-width int?)
(s/def ::map (s/keys :req [::block-width ::w ::h ::positions ::robot]))
(s/def ::command (s/tuple int? int?))
(s/def ::commands (s/coll-of ::command))
(s/def ::state (s/keys :req [::map ::commands]))
(s/conform ::position-stream {::w 10 ::h 12 ::raw-positions [{::p [0 1] ::kind ::wall} {::p [0 0] ::kind ::robot}]})


(s/conform ::position-stream
           #:com.benjaminbinford.day15{:w 8,
                                       :h 8,
                                       :raw-positions
                                       '(#:com.benjaminbinford.day15{:p [0 0], :kind :com.benjaminbinford.day15/wall}
                                         #:com.benjaminbinford.day15{:p [1 0], :kind :com.benjaminbinford.day15/wall}
                                         #:com.benjaminbinford.day15{:p [2 0], :kind :com.benjaminbinford.day15/wall}
                                         #:com.benjaminbinford.day15{:p [3 0], :kind :com.benjaminbinford.day15/wall})})

(defn position-stream [input]
  {:pre [(s/valid? string? input)]
   :post [(s/valid? ::position-stream %)]}
  (let [lines (str/split-lines input)]

    {::w (count (first lines))
     ::h (count lines)
     ::raw-positions (apply concat (map-indexed
                                    (fn [j line]
                                      (keep-indexed
                                       (fn [i c]
                                         (when (not= \. c)
                                           {::p [i j] ::kind (case c \# ::wall \O ::box \@ ::robot)})) line))
                                    lines))}))

(position-stream "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########")
;;=> #:com.benjaminbinford.day15{:w 8,
;;                               :h 8,
;;                               :raw-positions
;;                               (#:com.benjaminbinford.day15{:p [0 0], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [1 0], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [2 0], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [3 0], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [4 0], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [5 0], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [6 0], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [7 0], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [0 1], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [3 1], :kind :com.benjaminbinford.day15/box}
;;                                #:com.benjaminbinford.day15{:p [5 1], :kind :com.benjaminbinford.day15/box}
;;                                #:com.benjaminbinford.day15{:p [7 1], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [0 2], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [1 2], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [2 2], :kind :com.benjaminbinford.day15/robot}
;;                                #:com.benjaminbinford.day15{:p [4 2], :kind :com.benjaminbinford.day15/box}
;;                                #:com.benjaminbinford.day15{:p [7 2], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [0 3], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [4 3], :kind :com.benjaminbinford.day15/box}
;;                                #:com.benjaminbinford.day15{:p [7 3], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [0 4], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [2 4], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [4 4], :kind :com.benjaminbinford.day15/box}
;;                                #:com.benjaminbinford.day15{:p [7 4], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [0 5], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [4 5], :kind :com.benjaminbinford.day15/box}
;;                                #:com.benjaminbinford.day15{:p [7 5], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [0 6], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [7 6], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [0 7], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [1 7], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [2 7], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [3 7], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [4 7], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [5 7], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [6 7], :kind :com.benjaminbinford.day15/wall}
;;                                #:com.benjaminbinford.day15{:p [7 7], :kind :com.benjaminbinford.day15/wall})}



(defn parse-positions [input block-width]
  {:pre [(s/valid? string? input)
         (s/valid? int block-width)]
   :post [(s/valid? ::map %)]}
  (let [{:keys [::raw-positions ::w ::h]} (position-stream input)]

    (reduce
     (fn [{:keys [::positions ::robot]} pos]
       (let [new-positions (assoc positions (::p pos) (::kind pos))]
         (if (= ::robot (::kind pos))
           {::positions new-positions ::robot (::p pos)  ::w w ::h h ::block-width block-width}
           {::positions new-positions ::robot robot  ::w w ::h h ::block-width block-width})))
     {::positions {} ::robot nil ::w w ::h h ::block-width block-width}
     raw-positions)))

(parse-positions "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########" 2)
;;=> #:com.benjaminbinford.day15{:positions
;;                               {[7 6] :com.benjaminbinford.day15/wall,
;;                                [7 1] :com.benjaminbinford.day15/wall,
;;                                [4 3] :com.benjaminbinford.day15/box,
;;                                [2 2] :com.benjaminbinford.day15/robot,
;;                                [0 0] :com.benjaminbinford.day15/wall,
;;                                [7 7] :com.benjaminbinford.day15/wall,
;;                                [1 0] :com.benjaminbinford.day15/wall,
;;                                [7 2] :com.benjaminbinford.day15/wall,
;;                                [6 7] :com.benjaminbinford.day15/wall,
;;                                [7 4] :com.benjaminbinford.day15/wall,
;;                                [0 6] :com.benjaminbinford.day15/wall,
;;                                [0 5] :com.benjaminbinford.day15/wall,
;;                                [7 3] :com.benjaminbinford.day15/wall,
;;                                [4 2] :com.benjaminbinford.day15/box,
;;                                [3 0] :com.benjaminbinford.day15/wall,
;;                                [4 7] :com.benjaminbinford.day15/wall,
;;                                [5 7] :com.benjaminbinford.day15/wall,
;;                                [1 7] :com.benjaminbinford.day15/wall,
;;                                [0 3] :com.benjaminbinford.day15/wall,
;;                                [5 1] :com.benjaminbinford.day15/box,
;;                                [0 7] :com.benjaminbinford.day15/wall,
;;                                [2 7] :com.benjaminbinford.day15/wall,
;;                                [2 4] :com.benjaminbinford.day15/wall,
;;                                [4 5] :com.benjaminbinford.day15/box,
;;                                [7 0] :com.benjaminbinford.day15/wall,
;;                                [0 2] :com.benjaminbinford.day15/wall,
;;                                [2 0] :com.benjaminbinford.day15/wall,
;;                                [0 4] :com.benjaminbinford.day15/wall,
;;                                [3 1] :com.benjaminbinford.day15/box,
;;                                [4 4] :com.benjaminbinford.day15/box,
;;                                [3 7] :com.benjaminbinford.day15/wall,
;;                                [7 5] :com.benjaminbinford.day15/wall,
;;                                [5 0] :com.benjaminbinford.day15/wall,
;;                                [6 0] :com.benjaminbinford.day15/wall,
;;                                [1 2] :com.benjaminbinford.day15/wall,
;;                                [0 1] :com.benjaminbinford.day15/wall,
;;                                [4 0] :com.benjaminbinford.day15/wall},
;;                               :robot [2 2],
;;                               :w 8,
;;                               :h 8,
;;                               :block-width 2}


(defn parse-commands [input]
  {:pre [(s/valid? string? input)]
   :post [(s/valid? ::commands %)]}
  (vec (for [c input
             :when (#{\< \> \^ \v} c)]
         (cond (= c \<) [-1 0]
               (= c \>) [1 0]
               (= c \^) [0 -1]
               (= c \v) [0 1]))))

(parse-commands "<^^>>>vv<v>>v<<")
;;=> [[-1 0] [0 -1] [0 -1] [1 0] [1 0] [1 0] [0 1] [0 1] [-1 0] [0 1] [1 0] [1 0] [0 1] [-1 0] [-1 0]]

(defn parse [input block-width]
  {:pre [(s/valid? string? input)
         (s/valid? int block-width)]
   :post [(s/valid? ::state %)]}
  (let [sections (str/split input #"\n\n")]

    {::map (parse-positions (first sections) block-width)
     ::commands (parse-commands (second sections))}))


(def small-sample (parse "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<" 1))

small-sample
;;=> #:com.benjaminbinford.day15{:map
;;                               #:com.benjaminbinford.day15{:positions
;;                                                           {[7 6] :com.benjaminbinford.day15/wall,
;;                                                            [7 1] :com.benjaminbinford.day15/wall,
;;                                                            [4 3] :com.benjaminbinford.day15/box,
;;                                                            [2 2] :com.benjaminbinford.day15/robot,
;;                                                            [0 0] :com.benjaminbinford.day15/wall,
;;                                                            [7 7] :com.benjaminbinford.day15/wall,
;;                                                            [1 0] :com.benjaminbinford.day15/wall,
;;                                                            [7 2] :com.benjaminbinford.day15/wall,
;;                                                            [6 7] :com.benjaminbinford.day15/wall,
;;                                                            [7 4] :com.benjaminbinford.day15/wall,
;;                                                            [0 6] :com.benjaminbinford.day15/wall,
;;                                                            [0 5] :com.benjaminbinford.day15/wall,
;;                                                            [7 3] :com.benjaminbinford.day15/wall,
;;                                                            [4 2] :com.benjaminbinford.day15/box,
;;                                                            [3 0] :com.benjaminbinford.day15/wall,
;;                                                            [4 7] :com.benjaminbinford.day15/wall,
;;                                                            [5 7] :com.benjaminbinford.day15/wall,
;;                                                            [1 7] :com.benjaminbinford.day15/wall,
;;                                                            [0 3] :com.benjaminbinford.day15/wall,
;;                                                            [5 1] :com.benjaminbinford.day15/box,
;;                                                            [0 7] :com.benjaminbinford.day15/wall,
;;                                                            [2 7] :com.benjaminbinford.day15/wall,
;;                                                            [2 4] :com.benjaminbinford.day15/wall,
;;                                                            [4 5] :com.benjaminbinford.day15/box,
;;                                                            [7 0] :com.benjaminbinford.day15/wall,
;;                                                            [0 2] :com.benjaminbinford.day15/wall,
;;                                                            [2 0] :com.benjaminbinford.day15/wall,
;;                                                            [0 4] :com.benjaminbinford.day15/wall,
;;                                                            [3 1] :com.benjaminbinford.day15/box,
;;                                                            [4 4] :com.benjaminbinford.day15/box,
;;                                                            [3 7] :com.benjaminbinford.day15/wall,
;;                                                            [7 5] :com.benjaminbinford.day15/wall,
;;                                                            [5 0] :com.benjaminbinford.day15/wall,
;;                                                            [6 0] :com.benjaminbinford.day15/wall,
;;                                                            [1 2] :com.benjaminbinford.day15/wall,
;;                                                            [0 1] :com.benjaminbinford.day15/wall,
;;                                                            [4 0] :com.benjaminbinford.day15/wall},
;;                                                           :robot [2 2],
;;                                                           :w 8,
;;                                                           :h 8},
;;                               :commands
;;                               [[-1 0]
;;                                [0 -1]
;;                                [0 -1]
;;                                [1 0]
;;                                [1 0]
;;                                [1 0]
;;                                [0 1]
;;                                [0 1]
;;                                [-1 0]
;;                                [0 1]
;;                                [1 0]
;;                                [1 0]
;;                                [0 1]
;;                                [-1 0]
;;                                [-1 0]]}


(def input (parse (slurp "resources/input.txt") 1))
(def sample (parse (slurp "resources/sample.txt") 1))



(defn display-kind [kind block-width]
  (case kind
    ::wall \#
    ::box (if (= block-width 2) \[ \O)
    ::robot \@
    nil \.))

(defn display [{{:keys [::w ::h ::positions ::block-width]} ::map :as state}]
  {:pre [(s/valid? ::state state)]}
  (let [empty-buffer (transient (vec (repeat (* h w) \.)))
        index (fn [[i j]] (+ (* w j) i))
        buffer (persistent! (reduce
                             (fn [buffer [[i j] kind]]
                               (let [buffer (assoc! buffer (index [i j]) (display-kind kind block-width))]
                                 (if (and (= 2 block-width) (= kind ::box))
                                   (assoc! buffer (index [(inc i) j]) \])
                                   buffer)))
                             empty-buffer positions))]
    (doseq [line (partition w buffer)]
      (println (apply str line)))))

(defn move [positions old-pos delta]
  {:pre [(s/valid? ::positions positions)
         (s/valid? ::p old-pos)
         (s/valid? ::p delta)]
   :post [(s/valid? ::positions %)]}
  (let [kind (get positions old-pos)]
    (assoc positions old-pos nil (mapv + old-pos delta) kind)))

(move {[7 6] ::wall,
       [7 1] ::wall} [7 6] [7 7])
;;=> {[7 6] nil, [7 1] :wall, [7 7] :wall}

(defn find-moveable [positions pos delta]
  {:pre [(s/valid? ::positions positions)
         (s/valid? ::p pos)
         (s/valid? ::p delta)]
   :post [(s/valid? ::position-list %)]}
  (loop [moveable []
         pos pos]
    (let [new-pos (mapv + pos delta)
          kind (get positions new-pos)]
      (case kind
        ::wall [] ; we ran into a wall, no moves possible
        nil (reverse (conj moveable pos)) ; move the head of the line first to make room for everyone
        ::box (recur (conj moveable pos) new-pos)
        ::robot (recur (conj moveable pos) new-pos)))))

(find-moveable (get-in small-sample [::map ::positions]) [2 2] [-1 0])
;;=> []
(find-moveable (get-in small-sample [::map ::positions]) [2 2] [0 -1])
;;=> ([2 2])

(find-moveable (get-in small-sample [::map ::positions]) [4 5] [0 -1])
;;=> ([4 2] [4 3] [4 4] [4 5])

(defn move-all [positions moveable delta]
  {:pre [(s/valid? ::positions positions)
         (s/valid? ::position-list moveable)
         (s/valid? ::p delta)]
   :post [(s/valid? ::positions %)]}
  (reduce (fn [positions pos]
            (move positions pos delta))
          positions
          moveable))

(move-all (get-in small-sample [::map ::positions]) [[4 2] [4 3] [4 4] [4 5]] [0 -1])
;;=> {[7 6] :com.benjaminbinford.day15/wall,
;;    [7 1] :com.benjaminbinford.day15/wall,
;;    [4 3] :com.benjaminbinford.day15/box,
;;    [2 2] :com.benjaminbinford.day15/robot,
;;    [0 0] :com.benjaminbinford.day15/wall,
;;    [7 7] :com.benjaminbinford.day15/wall,
;;    [1 0] :com.benjaminbinford.day15/wall,
;;    [7 2] :com.benjaminbinford.day15/wall,
;;    [6 7] :com.benjaminbinford.day15/wall,
;;    [7 4] :com.benjaminbinford.day15/wall,
;;    [0 6] :com.benjaminbinford.day15/wall,
;;    [0 5] :com.benjaminbinford.day15/wall,
;;    [7 3] :com.benjaminbinford.day15/wall,
;;    [4 2] :com.benjaminbinford.day15/box,
;;    [3 0] :com.benjaminbinford.day15/wall,
;;    [4 7] :com.benjaminbinford.day15/wall,
;;    [4 1] :com.benjaminbinford.day15/box,
;;    [5 7] :com.benjaminbinford.day15/wall,
;;    [1 7] :com.benjaminbinford.day15/wall,
;;    [0 3] :com.benjaminbinford.day15/wall,
;;    [5 1] :com.benjaminbinford.day15/box,
;;    [0 7] :com.benjaminbinford.day15/wall,
;;    [2 7] :com.benjaminbinford.day15/wall,
;;    [2 4] :com.benjaminbinford.day15/wall,
;;    [4 5] nil,
;;    [7 0] :com.benjaminbinford.day15/wall,
;;    [0 2] :com.benjaminbinford.day15/wall,
;;    [2 0] :com.benjaminbinford.day15/wall,
;;    [0 4] :com.benjaminbinford.day15/wall,
;;    [3 1] :com.benjaminbinford.day15/box,
;;    [4 4] :com.benjaminbinford.day15/box,
;;    [3 7] :com.benjaminbinford.day15/wall,
;;    [7 5] :com.benjaminbinford.day15/wall,
;;    [5 0] :com.benjaminbinford.day15/wall,
;;    [6 0] :com.benjaminbinford.day15/wall,
;;    [1 2] :com.benjaminbinford.day15/wall,
;;    [0 1] :com.benjaminbinford.day15/wall,
;;    [4 0] :com.benjaminbinford.day15/wall}


(defn step-robot [map [di dj]]
  {:pre [(s/valid? ::map map)
         (s/valid? ::p [di dj])]
   :post [(s/valid? ::map %)]}
  (let [positions (get map ::positions)
        [ri rj] (get map ::robot)
        moveable (find-moveable positions [ri rj] [di dj])
        new-robot (if (empty? moveable) [ri rj] [(+ ri di) (+ rj dj)])
        new-positions (move-all positions moveable [di dj])
        new-map (assoc (assoc map ::robot new-robot) ::positions new-positions)]
    new-map))

(defn step [state]
  {:pre [(s/valid? ::state state)]
   :post [(s/valid? ::state %)]}
  (loop [state state]
    (let [{map ::map commands ::commands} state]
      (if (empty? commands)
        state
        (let [command (first commands)
              new-commands (rest commands)
              new-map (step-robot map command)]
          (recur {::map new-map
                  ::commands new-commands}))))))

(display (step small-sample))
;;=> nil


(display (step sample))

(defn gps [{{positions ::positions} ::map :as state}]
  {:pre [(s/valid? ::state state)]
   :post [(s/valid? number? %)]}
  (let [boxes (filter #(= ::box (second %)) positions)
        gps (map (fn [[[i j] _]] (+ i (* 100 j))) boxes)]

    (reduce + gps)))

sample

(gps (step sample))
;;=> 10092]
(gps (step input))
;;=> 1495147


(defn parse-expand-map [input]
  (let [double-walls (str/replace input #"#" "##")
        double-empty (str/replace double-walls #"\." "..")
        double-robot (str/replace double-empty #"@" "@.")
        double-box (str/replace double-robot #"O" "O.")]
    (parse double-box 2)))

(def day2small (parse-expand-map "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"))
day2small
;;=> #:com.benjaminbinford.day15{:map
;;                               #:com.benjaminbinford.day15{:positions
;;                                                           {[7 6] :com.benjaminbinford.day15/wall,
;;                                                            [12 6] :com.benjaminbinford.day15/wall,
;;                                                            [13 3] :com.benjaminbinford.day15/wall,
;;                                                            [0 0] :com.benjaminbinford.day15/wall,
;;                                                            [13 6] :com.benjaminbinford.day15/wall,
;;                                                            [1 0] :com.benjaminbinford.day15/wall,
;;                                                            [8 3] :com.benjaminbinford.day15/box,
;;                                                            [0 6] :com.benjaminbinford.day15/wall,
;;                                                            [1 1] :com.benjaminbinford.day15/wall,
;;                                                            [6 3] :com.benjaminbinford.day15/box,
;;                                                            [0 5] :com.benjaminbinford.day15/wall,
;;                                                            [11 0] :com.benjaminbinford.day15/wall,
;;                                                            [12 5] :com.benjaminbinford.day15/wall,
;;                                                            [8 6] :com.benjaminbinford.day15/wall,
;;                                                            [12 2] :com.benjaminbinford.day15/wall,
;;                                                            [13 2] :com.benjaminbinford.day15/wall,
;;                                                            [3 0] :com.benjaminbinford.day15/wall,
;;                                                            [9 0] :com.benjaminbinford.day15/wall,
;;                                                            [6 6] :com.benjaminbinford.day15/wall,
;;                                                            [9 6] :com.benjaminbinford.day15/wall,
;;                                                            [12 1] :com.benjaminbinford.day15/wall,
;;                                                            [13 1] :com.benjaminbinford.day15/wall,
;;                                                            [13 0] :com.benjaminbinford.day15/wall,
;;                                                            [8 0] :com.benjaminbinford.day15/wall,
;;                                                            [4 6] :com.benjaminbinford.day15/wall,
;;                                                            [1 4] :com.benjaminbinford.day15/wall,
;;                                                            [12 0] :com.benjaminbinford.day15/wall,
;;                                                            [10 0] :com.benjaminbinford.day15/wall,
;;                                                            [1 3] :com.benjaminbinford.day15/wall,
;;                                                            [1 5] :com.benjaminbinford.day15/wall,
;;                                                            [11 6] :com.benjaminbinford.day15/wall,
;;                                                            [12 4] :com.benjaminbinford.day15/wall,
;;                                                            [6 4] :com.benjaminbinford.day15/box,
;;                                                            [8 1] :com.benjaminbinford.day15/wall,
;;                                                            [0 3] :com.benjaminbinford.day15/wall,
;;                                                            [5 6] :com.benjaminbinford.day15/wall,
;;                                                            [13 5] :com.benjaminbinford.day15/wall,
;;                                                            [3 6] :com.benjaminbinford.day15/wall,
;;                                                            [10 6] :com.benjaminbinford.day15/wall,
;;                                                            [9 1] :com.benjaminbinford.day15/wall,
;;                                                            [7 0] :com.benjaminbinford.day15/wall,
;;                                                            [0 2] :com.benjaminbinford.day15/wall,
;;                                                            [2 0] :com.benjaminbinford.day15/wall,
;;                                                            [0 4] :com.benjaminbinford.day15/wall,
;;                                                            [12 3] :com.benjaminbinford.day15/wall,
;;                                                            [1 6] :com.benjaminbinford.day15/wall,
;;                                                            [2 6] :com.benjaminbinford.day15/wall,
;;                                                            [5 0] :com.benjaminbinford.day15/wall,
;;                                                            [13 4] :com.benjaminbinford.day15/wall,
;;                                                            [6 0] :com.benjaminbinford.day15/wall,
;;                                                            ...},
;;                                                           :robot [10 3],
;;                                                           :w 14,
;;                                                           :h 7,
;;                                                           :block-width 2},
;;                               :commands [[-1 0] [0 1] [0 1] [-1 0] [-1 0] [0 -1] [0 -1] [-1 0] [-1 0] [0 -1] [0 -1]]}


(display day2small)

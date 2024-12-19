(ns com.benjaminbinford.day17
  (:require
   [clojure.math :as math])
  (:gen-class))



(defn make-program [a b c opcodes]
  {:a a
   :b b
   :c c
   :instruction-pointer 0
   :output []
   :opcodes opcodes})

(def sample (make-program 729 0 0 [0,1,5,4,3,0]))
(def input (make-program 63281501 0 0 [2,4,1,5,7,5,4,5,0,3,1,6,5,5,3,0]))

sample
;;=> {:a 729, :b 0, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(defn combo [program opcode]
  (assert (<= 0 opcode 6))
  (case opcode
    (0 1 2 3) opcode
    4 (:a program)
    5 (:b program)
    6 (:c program)))

(combo sample 0)
;;=> 0
(combo sample 4)
;;=> 729

(defn adv [program opcode]
  (assoc program :a
         (quot (:a program)  (rationalize (math/pow 2 (combo program opcode))))))


(adv sample 0)
;;=> {:a 729, :b 0, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(adv sample 1)
;;=> {:a 364, :b 0, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(adv sample 3)
;;=> {:a 91, :b 0, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}



(defn big-or
  [f & r]
  (reduce (fn [acc v] (.or acc (biginteger v))) (biginteger f) r))


(defn big-xor
  [f & r]
  (reduce (fn [acc v] (.xor acc (biginteger v))) (biginteger f) r))


(defn bxl [program opcode]
  (assoc program :b
         (big-xor  (:b program) opcode)))

(bxl (bxl sample 1) 1)
;;=> {:a 729, :b 0, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}
;;=> {:a 729, :b 5, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}
;;=> {:a 729, :b 2, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}
;;=> {:a 729, :b 1, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(defn bst [program opcode]
  (assoc program :b
         (mod (combo program opcode) 8)))

(bst sample 2)
;;=> {:a 729, :b 2, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(bst (assoc sample :c 9) 6)
;;=> {:a 729, :b 1, :c 9, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(defn jnz [program opcode]
  (if (zero? (:a program))
    program
    (assoc program :instruction-pointer opcode)))

(jnz (assoc sample :instruction-pointer 10) 3)
;;=> {:a 729, :b 0, :c 0, :instruction-pointer 13, :output [], :opcodes [0 1 5 4 3 0]}

(defn bxc [program _]
  (assoc program :b
         (big-xor  (:b program)  (:c program))))

(bxc (assoc (assoc sample :c 3) :b 1) 0)
;;=> {:a 729, :b 2, :c 3, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(defn out [program opcode]
  (update program :output conj (mod (combo program opcode) 8)))

(out sample 0)
;;=> {:a 729, :b 0, :c 0, :instruction-pointer 0, :output [0], :opcodes [0 1 5 4 3 0]}
(out sample 4)
;;=> {:a 729, :b 0, :c 0, :instruction-pointer 0, :output [1], :opcodes [0 1 5 4 3 0]}

(defn bdv [program opcode]
  (assoc program :b
         (quot (:a program)  (rationalize (math/pow 2 (combo program opcode))))))


(bdv (assoc sample :b 10) 0)
;;=> {:a 729, :b 729, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(bdv (assoc sample :b 10) 1)
;;=> {:a 729, :b 364, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(bdv (assoc sample :b 10) 4)
;;=> {:a 729, :b 0, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}



(defn cdv [program opcode]
  (assoc program :c
         (quot (:a program)  (rationalize (math/pow 2 (combo program opcode))))))


(cdv (assoc sample :c 10) 0)
;;=> {:a 729, :b 0, :c 729, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(cdv (assoc sample :c 10) 1)
;;=> {:a 729, :b 0, :c 364, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}

(cdv (assoc sample :c 10) 4)
;;=> {:a 729, :b 0, :c 0, :instruction-pointer 0, :output [], :opcodes [0 1 5 4 3 0]}


(def dispatch-table {0 adv
                     1 bxl
                     2 bst
                     3 jnz
                     4 bxc
                     5 out
                     6 bdv
                     7 cdv})


(def op-names-table {0 "adv"
                     1 "bxl"
                     2 "bst"
                     3 "jnz"
                     4 "bxc"
                     5 "out"
                     6 "bdv"
                     7 "cdv"})

(def needs-combo {0 true
                  1 false
                  2 true
                  3 false
                  4 false
                  5 true
                  6 true
                  7 true})

(defn combo-string [op operand]
  (if (needs-combo op)
    (case operand
      (0 1 2 3) operand
      4 "a"
      5 "b"
      6 "c")
    operand))

(defn disassemble [program]

  (loop [opcodes (:opcodes program)]
    (if (empty? opcodes)
      (println "done")
      (do
        (println (str (first opcodes) " " (op-names-table (first opcodes)) " " (combo-string (first opcodes) (second opcodes)) " "))
        (recur (drop 2 opcodes))))))


(defn advance-ip [program save-ip]
  (update program
          :instruction-pointer
          #(if (= save-ip %)
             (+ 2 %)
             %)))

(defn step [program]
  (let [op (get-in program [:opcodes (:instruction-pointer program)])
        opcode (get-in program [:opcodes (inc (:instruction-pointer program))])
        ip-save (:instruction-pointer program)]
    (-> program
        ((dispatch-table op) opcode)
        (advance-ip ip-save))))

(step (make-program 0 0 9 [2 6]))
;;=> {:a 0, :b 1, :c 9, :instruction-pointer 2, :output [], :opcodes [2 6]}



(defn run [program]
  (if (>= (:instruction-pointer program) (count (:opcodes program)))
    (:output program)
    (recur (step program))))

(run  (make-program 0 0 9 [2 6]))
;;=> {:a 0, :b 1, :c 9, :instruction-pointer 2, :output [], :opcodes [2 6]}

(run  (make-program 10 0 0 [5,0,5,1,5,4]))
;;=> {:a 10, :b 0, :c 0, :instruction-pointer 6, :output [0 1 2], :opcodes [5 0 5 1 5 4]}

(run  (make-program 2024 0 0 [0,1,5,4,3,0]))
;;=> {:a 1012, :b 0, :c 0, :instruction-pointer 6, :output [4], :opcodes [0 1 5 4 3 0]}


(run  (make-program 0 29 0 [1,7]))
;;=> {:a 0, :b 26, :c 0, :instruction-pointer 2, :output [], :opcodes [1 7]}

(run  (make-program 0 2024 43690 [4,0]))
;;=> {:a 0, :b 44354, :c 43690, :instruction-pointer 2, :output [], :opcodes [4 0]}

(run sample)
;;=> {:a 0, :b 0, :c 0, :instruction-pointer 6, :output [4 6 3 5 6 3 5 2 1 0], :opcodes [0 1 5 4 3 0]}

(run input)
;;=> "3,4,3,1,7,6,5,6,0"

(run (make-program 2024 0 0 [0,3,5,4,3,0]))
;;=> "5,7,3,0"
(run (make-program 117440 0 0 [0,3,5,4,3,0]))
;;=> "0,3,5,4,3,0"

(meta adv)
;;=> nil

(def sample2 (make-program 2024 0 0 [0,3,5,4,3,0]))

(defn x [a]
  (count
   (take-while #(not (zero? %))
               (iterate (fn [x]
                          (quot x 8))
                        a))))

(defn find-first-16 [low high]
  (loop [low low
         high high]
    (let [mid (quot (+ low high) 2)
          result (x mid)]
      (cond
        (and (= result 16) (= 15 (x (dec mid)))) mid
        (> low high) nil
        (< result 16) (recur (inc mid) high)
        :else (recur low (dec mid))))))

(defn find-first-17 [low high]
  (loop [low low
         high high]
    (let [mid (quot (+ low high) 2)
          result (x mid)]
      (cond
        (and (= result 17) (= 16 (x (dec mid)))) mid
        (> low high) nil
        (< result 17) (recur (inc mid) high)
        :else (recur low (dec mid))))))

(def start-value 35184372088832)
68451429008588
(def max-value   281474976710656)
(def found-value 136902858017178)


(quot 35184372088832 (rationalize (math/pow 2 8)))
;;=> 137438953472N
(defn try-program [x opcodes]
  (let [program (make-program x 0 0 opcodes)]
    (when (= opcodes  (run program))
      x)))

(defn try-program-first-opcode [x opcodes]
  (let [program (make-program x 0 0 opcodes)]
    (when (= (first opcodes)  (first (run program)))
      x)))


(defn try-program-n-opcode [x n opcodes]
  (let [program (make-program x 0 0 opcodes)]
    (when (= (drop (- (count opcodes) n) opcodes)  (take n (run program)))
      x)))

(nth [1] 11)
(defn examine [x]
  (run (make-program x 0 0 [2,4,1,5,7,5,4,5,0,3,1,6,5,5,3,0])))

(try-program 2024 [0,3,5,4,3,0])
;;=> nil
(try-program 117440 [0,3,5,4,3,0])
;;=> 117440
(defn find-program [start-value opcodes]
  (loop [x start-value i 1]
    (when (zero? (mod i 100000))
      (println i))
    (if-let [result (try-program x opcodes)]
      result
      (if (< i 100000000)
        (recur (inc x) (inc i))
        (println i)))))

(defn find-program-first-opcode [start-value opcodes]
  (loop [x start-value i 1]
    (when (zero? (mod i 100000))
      (println i))
    (if-let [result (try-program-first-opcode x opcodes)]
      result
      (if (< i 100000000)
        (recur (inc x) (inc i))
        (println i)))))

(defn find-program-n-opcode [start-value n opcodes]
  (loop [x start-value i 1]
    (when (zero? (mod i 100000))
      (println i))
    (if-let [result (try-program-n-opcode x n opcodes)]
      result
      (if (< i 100000000)
        (recur (inc x) (inc i))
        (println i)))))


(find-program 7439 [0,3,5,4,3,0])
;;=> 117440

;; (run (make-program 117440 0 0 [0,3,5,4,3,0]))

;; (find-program 35184371900000 [2,4,1,5,7,5,4,5,0,3,1,6,5,5,3,0])
(run (make-program (dec start-value) 0 0 [2,4,1,5,7,5,4,5,0,3,1,6,5,5,3,0]))
;;=> Syntax error compiling at (src/com/benjaminbinford/day17.clj:280:1).
;;   Unable to resolve symbol: run in this context
;;   
;;=> [3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 5N]
;;=> [3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 3N 1N 3N 2N]
;;=> [0N 1N 5N 7N 7N 6N 3N 3N 3N 3N 3N 3N 3N 3N 5N]

;; (find-program start-value [2,4,1,5,7,5,4,5,0,3,1,6,5,5,3,0])


(def match-up-to-6 3287450)



; 2 bst a     a mod 8 -> b
; 1 bxl 5     b xor 0b101 -> b   
; 7 cdv b     a/(2^b) -> c
; 4 bxc 5     b xor c -> b
; 0 adv 3     a/8 -> a
; 1 bxl 6     b xor 0b110 -> b
; 5 out b     b -> output = 2
; 3 jnz 0 
; done
(defn input-expr [x acc]
  (if (zero? x)
    acc
    (let [a x
          b  (mod a 8)
          b (bit-xor b 5)
          c (bit-shift-right a b)
          b (big-xor b c)
          b (big-xor b 6)
          a (bit-shift-right a 3)]
      (recur a (conj acc (mod b 8))))))


(defn try-program-n-opcode [x n]
  (let [opcodes [2,4,1,5,7,5,4,5,0,3,1,6,5,5,3,0]]
    (when (= (drop (- (count opcodes) n) opcodes)  (take n (input-expr x [])))
      x)))
(try-program-n-opcode 8160041 8)
;;=> 8160041
(try-program-n-opcode 8160040 8)
;;=> nil

(defn extend-program [x n]
  (for [i (range 8)
        :let [x (try-program-n-opcode (+ (* x 8) i) (inc n))]
        :when x]
    [x (inc n)]))

(extend-program 0 0)
;;=> ([3 1])

(defn search-program []
  (loop [q [[0 0]] poss []]
    (if (empty? q)
      (apply min poss)
      (let [[x n] (first q)
            q (rest q)]
        (if (= n  16)
          (recur q (conj poss x))
          (recur (concat q (extend-program x n))
                 poss))))))
 

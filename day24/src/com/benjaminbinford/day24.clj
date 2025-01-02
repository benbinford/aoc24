(ns com.benjaminbinford.day24
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-values
  "parse a list values separted by \n of the form x01: 1 or cdw: 9"
  [rule-input]
  (into {} (map (fn [rule] (let [[key value] (str/split rule #": ")]
                             [key (Integer/parseInt value)]))
                (str/split rule-input #"\n"))))



(defn update-system [system key op in1 in2]
  ; (println "updating " key " with " in1 " " op " " in2)

  (let [value1 (get-in system [:values in1])
        value2 (get-in system [:values in2])
        deps1 (get-in system [:rules in1 :deps])
        deps2 (get-in system [:rules in2 :deps])]
    ; (println "new system " new-system)
    (-> system
        (assoc-in [:values key] (op value1 value2))
        (assoc-in [:rules key :deps] (concat deps1 deps2)))))

(defn evaluate-rule [{:keys [values] :as system} key]
  (cond
    (str/starts-with? key "x")
    (assoc-in system [:rules key :deps] [key])

    (str/starts-with? key "y")
    (assoc-in system [:rules key :deps] [key])

    (contains? values key)
    (do ;(println "retrieving present value for " key " " (get values key))
      system)

    :else
    (let [{:keys [in1 in2 op]} (get-in system [:rules key])
          op (cond
               (= op "AND") bit-and
               (= op "OR") bit-or
               (= op "XOR") bit-xor)]

      (-> system
          (evaluate-rule in1)
          (evaluate-rule in2)
          (update-system key op in1 in2)))))

;; (defn make-rule-evaluator [in1, in2, op, out]
;;   (fn [system]
;;     ; (println "evaluating " out " with " in1 " " op " " in2)
;;     (-> system
;;         (evaluate-rule in1)
;;         (evaluate-rule in2)
;;         (update-system out op in1 in2))))





(defn parse-rules
  "parse a list rules separted by \n of the form y18 XOR x18 -> wrn or 
   x09 AND y09 -> hww or pcd OR wff -> hbb"
  [rule-input]
  (into {} (map (fn [rule] (let [[rule-str, result] (str/split rule #" -> ")
                                 [gate1, op-string, gate2] (str/split rule-str #"\s")]
                             [result {:in1 gate1
                                      :in2 gate2
                                      :op op-string
                                      :result result}]))
                (str/split rule-input #"\n"))))

(defn parse [input]
  ((fn [[value-string, rule-string]] {:result 0
                                      :values (parse-values value-string)
                                      :rules  (parse-rules rule-string)})
   (str/split input #"\n\n")))


(def sample (parse (slurp "resources/sample.txt")))
(def sample2 (parse (slurp "resources/sample2.txt")))
(def input (parse (slurp "resources/input.txt")))


(defn valid? [{:keys [values rules]} key]
  (or (contains? values key)
      (contains? rules key)))


(defn format-gate [prefix n]
  (str prefix (format "%02d" n)))

(defn generate-keys [system prefix]
  (keep #(when (valid? system (second %)) %)
        (map (fn [x] [x (format-gate prefix x)]) (reverse (range 100)))))

(defn generate-z-values [system]
  (generate-keys system "z"))

(defn setup-operand-gates [system gate value]
  (let [keys (reverse (generate-keys system gate))]
    (loop [system system
           keys keys
           value value]
      (if (empty? keys)
        system
        (let [[_ key] (first keys)]
          (recur (assoc-in system [:values key] (bit-and 1 value))
                 (rest keys)
                 (quot value 2)))))))



(defn solve [system]
  (reduce
   (fn [system [_ z-gate]]
                ;(println "reducing " z-gate " to " (get-in system [:values z-gate]) " with result " (get-in system [:result]))
     (let [system (evaluate-rule system z-gate)]
       (update system :result
               #(+ (* %1 2) %2)
               (get-in system [:values z-gate]))))
   system
   (generate-z-values system)))

(defn part1 [system]
  (:result (solve system)))

(part1 input)

(filter #(and (or (str/starts-with? (:in1 (second %)) "x")
                  (str/starts-with? (:in1 (second %)) "y"))
              (or (str/starts-with? (:in2 (second %)) "x")
                  (str/starts-with? (:in2 (second %)) "y"))
              (str/starts-with? (first %) "z")) (:rules input))

(defn print-tree
  "Print out a depth first traversal of the tree showing operands and one op per line. 
  The depth of the tree will be represented by spacing at the start of lines"
  ([system rule-name depth]
   (let [rule (get-in system [:rules rule-name])]
     (print (str/join (repeat depth " ")) rule-name)
     (if rule
       (do
         (println " " (:op rule))
         (print-tree system (:in1 rule) (+ 5 depth))
         (print-tree system (:in2 rule) (+ 5 depth)))
       (print "\n"))))


  ([system rule]
   (print-tree system rule 0)))




;;;  Zn = XOR (XOR Xn Yn) (Carry Zn-1)
 ;; ZO = XOR X0 Y0
;; Z1 = XOR (XOR X1 Y1) (CARRY Z0)
;; Z2 = XOR (XOR X2 Y2) (CARRY Z1)

;;; Carry Z0 = 0
;;; Carry Z1 = AND X0 0
;;; Carry Z2 = OR (XOR X1 X1) (CARRY Z1)
;;; Carry Z3 = OR (AND X2 Y2) (AND (X2 XOR Y2) (OR (XOR X1 Y1) (CARRY Z1)))
;; CARRY Z3 = OR (AND X2 Y2) (AND (X2 XOR Y2) (CARRY Z2))

;;CARRY Zn = OR (AND Xn-1 Yn-1) (AND (Xn-1 XOR Yn-1) (CARRY Zn-1))

;; clj꞉com.benjaminbinford.day24꞉>  (print-tree input "z00")
;; ;  z00  XOR
;; ;       y00
;; ;       x00
;; nil
;; clj꞉com.benjaminbinford.day24꞉> 
;; clj꞉com.benjaminbinford.day24꞉>  (print-tree input "z01")
;; ;  z01  XOR
;; ;       wsg  AND
;; ;            y00
;; ;            x00
;; ;       pnw  XOR
;; ;            y01
;; ;            x01
;; nil
;; clj꞉com.benjaminbinford.day24꞉>  (print-tree input "z02")
;; ;  z02  XOR
;; ;       mts  XOR
;; ;            x02
;; ;            y02
;; ;       ntr  OR
;; ;            wwc  AND
;; ;                 x01
;; ;                 y01
;; ;            hch  AND
;; ;                 wsg  AND
;; ;                      y00
;; ;                      x00
;; ;                 pnw  XOR
;; ;                      y01
;; ;                      x01
;; nil
;; clj꞉com.benjaminbinford.day24꞉>  (print-tree input "z03")
;; ;  z03  XOR
;; ;       shd  OR
;; ;            qhg  AND
;; ;                 x02
;; ;                 y02
;; ;            jgj  AND
;; ;                 mts  XOR
;; ;                      x02
;; ;                      y02
;; ;                 ntr  OR
;; ;                      wwc  AND
;; ;                           x01
;; ;                           y01
;; ;                      hch  AND
;; ;                           wsg  AND
;; ;                                y00
;; ;                                x00
;; ;                           pnw  XOR
;; ;                                y01
;; ;                                x01
;; ;       tkd  XOR
;; ;            y03
;; ;            x03
;; nil
;; clj꞉com.benjaminbinford.day24꞉> 
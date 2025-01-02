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


;; (defn remove-inderect-rule [system key rule]
;;   (println "removing " key rule)
;;   (cond
;;     (not (string? key))
;;     [system rule]


;;     (not rule)
;;     [system key]

;;     :else
;;     (let [{:keys [in1 in2]} rule
;;           [system in1] (remove-inderect-rule system in1 (get-in system [:rules in1]))
;;           [system in2] (remove-inderect-rule system in2 (get-in system [:rules in2]))
;;           rule (assoc rule :in1 in1)
;;           rule (assoc rule :in2 in2)
;;           system (assoc-in system [:rules key] rule)]
;;       (println "updating " key " with " in1 " " in2)

;;       (println "after update " system)
;;       [system rule])))


;; (defn remove-indirect-rules [system]
;;   (reduce
;;    (fn [next-system [key, rule]]
;;      (first (remove-inderect-rule next-system key rule)))
;;    system
;;    (:rules system)))

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


(defn matches-sum-n [_ index {:keys [in1 in2 op]}  key]
  (if (and (= "XOR" op)
           (or (and (= in1 (format-gate "x" index)) (= in2 (format-gate "y" index)))
               (and (= in1 (format-gate "y" index)) (= in2 (format-gate "x" index)))))
    []
    [{:key key :index index :rule "matches-sum-n"}]))



(defn matches-half-carry-n [_ index {:keys [in1 in2 op]}  key]
  (if (and (= "AND" op)
           (or (and (= in1 (format-gate "x" index)) (= in2 (format-gate "y" index)))
               (and (= in1 (format-gate "y" index)) (= in2 (format-gate "x" index)))))
    []
    [{:key key :index index :rule "matches-half-carry-n"}]))



;;CARRY Zn = OR (AND Xn-1 Yn-1) (AND (Xn-1 XOR Yn-1) (CARRY Zn-1))
(declare matches-prior-carries-n)
(declare matches-rule)

(defn matches-carry-n [system index {:keys [in1 in2 op] :as rule}  key]
  (cond
    (= 0 index) []
    (= 1 index)
    (matches-half-carry-n system (dec index) rule key)
    :else
    (matches-rule system key index "matches-carry-n"
                  "OR"
                  (partial matches-half-carry-n system (dec index))
                  (partial matches-prior-carries-n system (dec index)))))


(defn matches-prior-carries-n [system index _  key]
  (matches-rule system key index "matches-prior-carries-n"
                "AND"
                (partial matches-sum-n system index)
                (partial matches-carry-n system index)))

(defn matches-rule [system key index context expected-op op-rule-a op-rule-b]
  (let [{:keys [in1 in2 op]} (get-in system [:rules key])]
    (if (not= op expected-op)
      [{:key key :index index :rule (str context " expected " expected-op " but got " op)}]
      (let [in1-rule (get-in system [:rules in1])
            in2-rule (get-in system [:rules in2])
            bad-in1-rule-a (op-rule-a in1-rule in1)
            bad-in2-rule-a (op-rule-a in2-rule in2)
            bad-in1-rule-b (op-rule-b in1-rule in1)
            bad-in2-rule-b (op-rule-b in2-rule in2)]
        (cond
          (empty? bad-in1-rule-a)

          (if (empty? bad-in2-rule-b)
            []
            bad-in2-rule-b)

          (empty? bad-in2-rule-a)

          (if (empty? bad-in1-rule-b)
            []
            bad-in1-rule-b)

          (empty? bad-in1-rule-b)
          bad-in2-rule-a

          (empty? bad-in2-rule-b)
          bad-in1-rule-a

          :else
          [{:key key :index index :rule (str context " failed to match children")}])))))


(defn correct-zn [system index key]
  (let [rule (get-in system [:rules key])]
    (if (= 0 index)
      (matches-sum-n system index rule  key)
      (matches-rule system key index "correct-zn"  "XOR" (partial matches-sum-n system index) (partial matches-carry-n system index)))))


(correct-zn input 5 "z20")
;;=> ["z05"]
;;=> [5]
(get-in input [:rules "z00"])

(into #{} (mapcat #(correct-zn input (first %) (second %)) (generate-z-values input)))
;;; after hhh/z20
;;=> #{{:key "z45", :index 45, :rule "correct-zn expected XOR but got OR"}}


;; and z45 is just the carry


;;z45 is the carry for z45
;;z20 is the carry for z21

;z15 is part of a carry tree for z16+
;;; after htp/z15
;;=> #{{:key "z45", :index 45, :rule "correct-zn expected XOR but got OR"}
;;     {:key "z20", :index 20, :rule "correct-zn expected XOR but got OR"}
;;     {:key "hhh", :index 21, :rule "matches-carry-n expected OR but got XOR"}}


;;; after dkr/z05

;;=> #{{:key "htp", :index 15, :rule "matches-prior-carries-n expected AND but got XOR"}
;;     {:key "z45", :index 45, :rule "correct-zn expected XOR but got OR"}
;;     {:key "z15", :index 15, :rule "correct-zn expected XOR but got AND"}
;;     {:key "z20", :index 20, :rule "correct-zn expected XOR but got OR"}
;;     {:key "hhh", :index 21, :rule "matches-carry-n expected OR but got XOR"}}


;;; after rhv/ggk
;;=> #{{:key "htp", :index 15, :rule "matches-prior-carries-n expected AND but got XOR"}
;;     {:key "z45", :index 45, :rule "correct-zn expected XOR but got OR"}
;;     {:key "z05", :index 5, :rule "correct-zn expected XOR but got AND"}
;;     {:key "z15", :index 15, :rule "correct-zn expected XOR but got AND"}
;;     {:key "z20", :index 20, :rule "correct-zn expected XOR but got OR"}
;;     {:key "dkr", :index 5, :rule "matches-half-carry-n"}
;;     {:key "hhh", :index 21, :rule "matches-carry-n expected OR but got XOR"}}


;;; original
;;=> #{{:key "htp", :index 15, :rule "matches-prior-carries-n expected AND but got XOR"}
;;     {:key "z45", :index 45, :rule "correct-zn expected XOR but got OR"}
;;     {:key "z05", :index 5, :rule "correct-zn expected XOR but got AND"}
;;     {:key "gqf", :index 37, :rule "matches-carry-n failed to match children"}
;;     {:key "z15", :index 15, :rule "correct-zn expected XOR but got AND"}
;;     {:key "z20", :index 20, :rule "correct-zn expected XOR but got OR"}
;;     {:key "dkr", :index 5, :rule "matches-half-carry-n"}
;;     {:key "z36", :index 36, :rule "correct-zn failed to match children"}
;;     {:key "hhh", :index 21, :rule "matches-carry-n expected OR but got XOR"}}



;;=> #{{:key "htp", :index 15, :rule "matches-prior-carries-n expected AND but got XOR"}
;;     {:key "dkr", :index 5, :rule "matches-half-carry-n"}
;;     {:key "gqf", :index 37, :rule "matches-carry-n failed to match children"}
;;     {:key "z45", :index 45, :rule "correct-zn expected XOR but got OR"}
;;     {:key "z05", :index 5, :rule "correct-zn expected XOR but got AND"}
;;     {:key "z15", :index 15, :rule "correct-zn expected XOR but got AND"}
;;     {:key "z20", :index 20, :rule "correct-zn expected XOR but got OR"}

;; ggk
;; hpg

;; bqf

;;;swaps
;; rhv/ggk
;; dkr/z05
;; htp/z15
;; hhh/z20


(str/join "," (sort ["hhh" "z20" "rhv" "ggk" "dkr" "z05" "htp" "z15"]))
;;=> "dkr,ggk,hhh,htp,rhv,z05,z15,z20"
(get-in input [:rules "z36"])
;;=> {:in1 "ggk", :in2 "hpg", :op "XOR", :result "z36"}

;;     {:key "z36", :index 36, :rule "correct-zn failed to match children"}
;;  

;; z36, z45 z05, z15, z20

;;     {:key "dkr", :index 5, :rule "matches-half-carry-n"}


;;;  Zn = XOR (XOR Xn Yn) (Carry Zn-1)
 ;; ZO = XOR X0 Y0
;; Z1 = XOR (XOR X1 Y1) (CARRY Z0)
;; Z2 = XOR (XOR X2 Y2) (CARRY Z1)

;;; Carry Z1 = 0
;;; Carry Z2 = AND X0 Y0
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
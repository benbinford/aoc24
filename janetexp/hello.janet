
(def sample-input `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
`)



(def line
  '(group (* (some (* :a* (<- :d))) :a* (? "\n"))))

(peg/match line "1a3bc2")

(defn combine-digits [digits]
  (pp digits)
  (pp "sep")
  (scan-number (string (first digits) (last digits))))

(combine-digits [1 2 3])

(def grammar2
  (peg/compile
   ~(* (some
        (replace  ,line ,combine-digits)) 
       (drop ($)))))

(sum (peg/match grammar2 sample-input))

(sum (peg/match grammar2 (slurp "input.txt")))


(def grammar
  (peg/compile
    ~{:main (some (* :line (? "\n")))
      :line (*
             (any :not-digit)
             (<- :digit :first-digit)
             (some (* (any :not-digit) (<- :digit :last-digit)))
             (any :not-digit))
      :digit :d
      :not-digit (not (+ "\n" :digit))}))

(defn parse-input [input]
  (let [result (peg/match grammar input)]
    (map (fn [line]
           [(get line :first-digit) (get line :last-digit)])
         (get result :line))))

(print (parse-input sample-input))
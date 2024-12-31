(ns com.benjaminbinford.day22
  (:gen-class))


(defn mix-and-prune [[secret value]]
  (mod (bit-xor secret value) 16777216))


(defn mul [secret value]
  [secret (* secret value)])

(defn div [secret value]
  [secret (quot secret value)])

(defn evolve-secret [secret]

  (-> secret
      (mul 64)
      mix-and-prune
      (div 32)
      mix-and-prune
      (mul 2048)
      mix-and-prune))

(evolve-secret 123)

(defn evolve-secrets [secret]
  (iterate evolve-secret  secret))
;; (take 10 (evolve-secrets 123))
;; ;;=> (15887950 16495136 527345 704524 1553684 12683156 11100544 12249484 7753432 5908254)


;; (take 10 (evolve-secrets 123))

;; (nth  (evolve-secrets 123) 10)

;; (nth  (evolve-secrets 1) 2000)


;; (reduce + 0 (map #(nth (evolve-secrets %) 2000) [1 10 100 2024]))
;; ;;=> 37327623


(def sample [1 10 100 2024])

(def input (read-string (slurp "resources/input.txt")))

(defn part1 [input]
  (reduce + 0 (map #(nth (evolve-secrets %) 2000) input)))


;; (part1 sample)
;;=> 37327623

;; (part1 input)
;;=> 12759339434
       


(ns com.benjaminbinford.day0
  (:gen-class)
  (:require
   [clojure.string]))

(defn greet
  "Callable entry point to the application."
  [data]

  (println #break (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))


(def ^:dynamic *input* (slurp "resources/sample.txt"))

(defn first-and-last [[f _] c]
  (let [newDigit (Integer/parseInt c)]
    (if (nil? f)
      [newDigit newDigit]
      [f newDigit])))

(first-and-last [2 2] "1")

(for
 [line (clojure.string/split *input* #"\n")]
  (reduce first-and-last
          [nil nil]
          (for [c (seq line) :where (Character/isDigit c)] c)))
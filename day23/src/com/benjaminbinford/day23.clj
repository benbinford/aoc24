(ns com.benjaminbinford.day23
  (:require [clojure.string :as str])
  (:gen-class))

(defn assoc-list [m k v]
  (assoc m k (conj (get m k #{}) v)))

(defn parse [input]
  (reduce #(let [[c1 c2] (str/split %2 #"-")]
             (->
              %1
              (assoc-list c1 c2)
              (assoc-list c2 c1)))
          {}
          (str/split input #"\n")))

(def sample (parse (slurp "resources/sample.txt")))

(def input (parse (slurp "resources/input.txt")))


(defn find-connected-triplets [m [c1 cs]]
  (into #{} (for [c2 cs
                  c3 (rest cs)
                  :let [c2-edges (get m c2 #{})
                        c3-edges (get m c3 #{})]
                  :when (and (c2-edges c3) (c3-edges c2))]
              #{c1 c2 c3})))

(find-connected-triplets sample ["aq" (get sample "aq")])
;;=> #{#{"wq" "vc" "aq"} #{"cg" "aq" "yn"}}

(defn find-all-connected-triplets [m p]
  (reduce (fn [acc c]
            (let [connected-triplets (find-connected-triplets m c)]
              (if (empty? connected-triplets)
                acc
                (into acc (filter p connected-triplets)))))
          #{}
          m))
(defn has-t? [triplet]
  (some #{\t} (apply str (map #(subs % 0 1) triplet))))


(count (find-all-connected-triplets sample has-t?))
;;=> 7
;;=> #{#{"co" "ka" "ta"}
;;     #{"de" "ka" "ta"}
;;     #{"td" "yn" "wh"}
;;     #{"co" "de" "ta"}
;;     #{"td" "tc" "wh"}
;;     #{"wq" "tb" "vc"}
;;     #{"td" "wh" "qp"}}

;;=> #{#{"co" "ka" "ta"}
;;     #{"kh" "qp" "ub"}
;;     #{"de" "ka" "ta"}
;;     #{"co" "de" "ka"}
;;     #{"td" "yn" "wh"}
;;     #{"co" "de" "ta"}
;;     #{"wq" "vc" "ub"}
;;     #{"td" "tc" "wh"}
;;     #{"wq" "vc" "aq"}
;;     #{"wq" "tb" "vc"}
;;     #{"cg" "aq" "yn"}
;;     #{"td" "wh" "qp"}}


(count (find-all-connected-triplets input has-t?))
;;=> 1366
;;=> 2377
;;=> #{#{"wt" "vg" "yi"}
;;     #{"ku" "tu" "ih"}
;;     #{"dt" "gs" "tn"}
;;     #{"st" "bs" "qo"}
;;     #{"pt" "ub" "gy"}
;;     #{"td" "vd" "cs"}
;;     #{"mt" "wz" "nr"}
;;     #{"tv" "zg" "qb"}
;;     #{"hs" "cd" "th"}
;;     #{"sq" "tl" "jn"}
;;     #{"uw" "wt" "xf"}
;;     #{"sq" "vq" "ta"}
;;     #{"tw" "lz" "fw"}
;;     #{"tg" "ky" "wg"}
;;     #{"mh" "te" "pb"}
;;     #{"qj" "nt" "ni"}
;;     #{"td" "kq" "cs"}
;;     #{"ht" "jv" "am"}
;;     #{"rm" "jt" "zj"}
;;     #{"un" "dy" "ft"}
;;     #{"lr" "tu" "dg"}
;;     #{"vn" "bu" "et"}
;;     #{"zs" "ou" "zt"}
;;     #{"ax" "au" "ty"}
;;     #{"wn" "sy" "tx"}
;;     #{"ac" "bw" "tl"}
;;     #{"vb" "tz" "wz"}
;;     #{"sc" "kt" "au"}
;;     #{"kd" "uo" "ft"}
;;     #{"gi" "tp" "yo"}
;;     #{"ht" "jv" "qh"}
;;     #{"pj" "nt" "bo"}
;;     #{"yt" "rv" "zl"}
;;     #{"nc" "tx" "ce"}
;;     #{"gn" "lf" "et"}
;;     #{"hs" "ht" "mr"}
;;     #{"hf" "mt" "nv"}
;;     #{"tn" "pq" "qc"}
;;     #{"cy" "bt" "vm"}
;;     #{"tw" "hh" "fw"}
;;     #{"hr" "tv" "ye"}
;;     #{"ad" "bt" "tf"}
;;     #{"hk" "kt" "li"}
;;     #{"pt" "ev" "gy"}
;;     #{"ti" "qo" "gk"}
;;     #{"nc" "sy" "tl"}
;;     #{"ra" "qh" "th"}
;;     #{"ht" "hy" "am"}
;;     #{"it" "gu" "gi"}
;;     #{"ht" "hy" "cd"}
;;     ...}
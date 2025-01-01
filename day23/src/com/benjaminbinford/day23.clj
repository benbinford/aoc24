(ns com.benjaminbinford.day23
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo])
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





(find-all-connected-triplets input identity)


(defn all-connected? [m cs]
  (when (every? (fn [[c1 c2]]
                  (let [c1-edges (get m c1 #{})
                        c2-edges (get m c2 #{})]
                    (and (c1-edges c2) (c2-edges c1))))
                (combo/combinations cs 2))
    cs))

(defn has-n-connections? [m n [_ cs]]

  (let [edges (combo/combinations cs n)]
    ;; for each partition, 
    (some (partial all-connected? m) edges)))

(has-n-connections? sample 3 ["ka" (get sample "ka")])

(defn best-connection [m [c1 cs]]
  (loop [n (count cs)]
    (if-let [connections (has-n-connections? m n [c1 cs])]
      (concat [c1] connections)
      (recur (dec n)))))


(best-connection sample ["ka" (get sample "ka")])
;;=> ("co" "de" "ta")



(defn find-all-connected [m]
  (str/join "," (sort (first (sort-by count > (map (partial best-connection m) m))))))

(find-all-connected input)
;;=> "bs,cf,cn,gb,gk,jf,mp,qk,qo,st,ti,uc,xw"

(str/join "," (sort (into [] (:computers (first (sort-by :edges  > (find-all-connected sample)))))))
;;=> "aq,tb,ub,vc,wq"

(find-all-connected sample)
;;=> [{:computers #{"cg" "tb" "de" "aq" "yn"}, :edges #{nil #{"aq" "yn"}}}
;;    {:computers #{"wq" "tb" "vc" "aq" "ub"}, :edges #{nil #{"vc" "aq"} #{"vc" "ub"} #{"tb" "vc"}}}
;;    {:computers #{"kh" "tc" "qp" "ta" "ub"}, :edges #{nil #{"qp" "ub"}}}
;;    {:computers #{"td" "yn" "tc" "wh" "qp"}, :edges #{nil #{"yn" "wh"} #{"tc" "wh"} #{"wh" "qp"}}}
;;    {:computers #{"cg" "wq" "tb" "vc" "ka"}, :edges #{nil #{"wq" "vc"}}}
;;    {:computers #{"wq" "tb" "vc" "aq" "ub"}, :edges #{nil #{"wq" "ub"} #{"wq" "tb"} #{"wq" "aq"}}}
;;    {:computers #{"co" "de" "tc" "ka" "ta"}, :edges #{nil #{"de" "ta"} #{"ka" "ta"} #{"de" "ka"}}}
;;    {:computers #{"cg" "co" "de" "ka" "ta"}, :edges #{nil #{"co" "ta"} #{"co" "ka"} #{"ka" "ta"}}}
;;    {:computers #{"cg" "wq" "vc" "aq" "yn"}, :edges #{nil #{"cg" "yn"} #{"wq" "vc"}}}
;;    {:computers #{"cg" "td" "aq" "yn" "wh"}, :edges #{nil #{"cg" "aq"} #{"td" "wh"}}}
;;    {:computers #{"kh" "td" "co" "tc" "wh"}, :edges #{nil #{"td" "wh"}}}
;;    {:computers #{"td" "yn" "tc" "wh" "qp"}, :edges #{nil #{"td" "yn"} #{"td" "tc"} #{"td" "qp"}}}
;;    {:computers #{"tb" "co" "de" "ka" "ta"}, :edges #{nil #{"de" "ta"} #{"co" "de"} #{"co" "ta"}}}
;;    {:computers #{"kh" "td" "wh" "qp" "ub"}, :edges #{nil #{"td" "wh"} #{"kh" "ub"}}}
;;    {:computers #{"kh" "co" "de" "ka" "ta"}, :edges #{nil #{"co" "de"} #{"co" "ka"} #{"de" "ka"}}}
;;    {:computers #{"wq" "kh" "vc" "qp" "ub"}, :edges #{nil #{"kh" "qp"} #{"wq" "vc"}}}]
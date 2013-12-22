;; The 21st International Mathematical Olympiad (1979) posed the following problem:
;;   A prism with pentagons A_1 A_2 A_3 A_4 A_5 and B_1 B_2 B_3 B_4 B_5 as the top and
;;   bottom faces is given. Each side of the two pentagons and each of the 25 segments
;;   A_i B_j is colored red or green. Every triangle whose vertices are vertices of the
;;   prism and whose sides have all been colored has two sides of a different color.
;;   Prove that all 10 sides of the top and bottom faces have the same color.
;;
;; This program proves this statement by exhaustion.

(ns imo21_a2.core
  (:refer-clojure :exclude [==])
  (:use (clojure.core logic))
  (:gen-class))

(defn polychromatic
  "(polychromatic c0 c1 c2) holds iff c0, c1, c2 are not all the same color."
  [c0 c1 c2]
  (let [t [c0 c1 c2]] (conde ((== t [:green :green :red]))
                             ((== t [:green :red :green]))
                             ((== t [:red :green :green]))
                             ((== t [:red :red :green]))
                             ((== t [:red :green :red]))
                             ((== t [:green :red :red])))))

(defn succ [u] (mod (+ u 1) 5))

(defn at
  "(at p u c) holds iff side u of the pentagon p has color c."
  [p u c]
  (fresh [c0 c1 c2 c3 c4]
         (== [c0 c1 c2 c3 c4] p)
         (== c ([c0 c1 c2 c3 c4] u))))

(defn at-int 
  "(at-int p u v c) holds iff the edge in p connecting topvertex u to
   bottomvertex v has color c."
  [p u v c]
  (fresh [int-c]
         (at p u int-c)
         (at int-c v c)))

(defn triangle-polychromatic
  "(triangle-polychromatic prism [u v]) holds iff the triangles
   {top-u, top-(u+1), bottom-v} and {top-u, bottom-v, bottom-(v+1)}
   are both not monochromatic."
  [prism [u v]]
  (fresh [top btm int c0 c1 c2 c3 c4]
         (== {:top top :btm btm :int int} prism)
         (at top u c1)
         (at btm v c2)
         (at-int int u v c0)
         (at-int int u (succ v) c3)
         (at-int int (succ u) v c4)
         (polychromatic c0 c1 c4)
         (polychromatic c0 c2 c3)))


(def top-btm-edges (for [u (range 5), v (range 5)] [u v]))

(defn triangles-polychromatic
  "(triangles-polychromatic prism) holds iff prism is a prism
   satisfying the conditions of the exercise."
  [prism]
  (everyg (partial triangle-polychromatic prism) top-btm-edges))

;; project the top and bottom pentagons out of a full prism
(def solution (tabled [top btm] (fresh [prism int]
                                       (triangles-polychromatic prism)
                                       (== {:top top :btm btm :int int} prism))))

(defn -main [] (doseq [sol (run* [top btm] (solution top btm))] (println sol)))

(ns rameezk.aoc-2020.day09
  (:require [clojure.string :as str]))

(def example-input (->> "data/day09-example.txt" slurp str/split-lines (map #(Long/parseLong %))))
(def real-input (->> "data/day09.txt" slurp str/split-lines (map #(Long/parseLong %))))

(defn- pair? [sum numbers]
  (pos?
    (count
      (for [n1    numbers
            n2    (rest numbers)
            :when (= sum (+ n1 n2))]
        [n1 n2]))))

(defn attack [preamble-size input]
  (let [pts (partition-all (inc preamble-size) 1 input)]
    (first (for [p     pts
                 :let  [n (last p)
                        preamble (butlast p)]
                 :when (not (pair? n preamble))]
             n))))

(defn- min-max-sum [window]
  (let [w (sort window)]
    (+ (first w) (last w))))

(defn attack2 [failed-num numbers]
  (loop [window numbers]
    (let [sums    (reductions + window)
          idx-sum (.indexOf sums failed-num)]
      (cond
        (>= idx-sum 0)        (min-max-sum (take (inc idx-sum) window))
        (pos? (count window)) (recur (drop 1 window))
        :else                 nil))))

(time (attack 25 real-input))
;; => 90433990

(time (attack2 (attack 25 real-input) real-input))
;; => 11691646




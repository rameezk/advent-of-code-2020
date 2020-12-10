(ns rameezk.aoc-2020.day10
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math :refer [expt]]))

(def short-example-input
  (->> "16
10
15
5
1
11
7
19
6
12
4" str/split-lines (map #(Long/parseLong %))))

(def example-input
  (->> "data/day10-example.txt"
       slurp
       str/split-lines
       (map #(Long/parseLong %))))

(def real-input
  (->> "data/day10.txt"
       slurp
       str/split-lines
       (map #(Long/parseLong %))))

(defn- built-in-adapter [input]
  (+ 3 (apply max input)))


(defn p1 [input]
  (let [device-joltage    (built-in-adapter input)
        {ones 1 threes 3} (->> (conj input 0 device-joltage)
                               sort
                               (partition 2 1)
                               (map #(- (second %) (first %)))
                               frequencies)]
    (* ones threes)))

(p1 real-input)
;; => 2380

(defn- power [x exp]
  (let [exp (if (nil? exp) 0 exp)]
    (expt x exp)))

(defn p2 [input]
  (let [device-joltage (built-in-adapter input)
        {a [1 1]
         b [1 1 1]
         c [1 1 1 1]}  (->> (conj input 0 device-joltage)
                            sort
                            (partition 2 1)
                            (map #(- (second %) (first %)))
                            (partition-by #(= 3 %))
                            frequencies)]
    (* (power 2 a) (power 4 b) (power 7 c))))

(p2 real-input)
;; => 48358655787008

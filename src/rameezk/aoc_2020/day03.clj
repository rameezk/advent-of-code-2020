(ns rameezk.aoc-2020.day03
  (:require [clojure.string :as str]))

(def file "data/day03.txt")

(def tree \#)

(defn- tree? [char]
  (= char tree))

(defn- extend-row [row times]
  (apply str (repeat times row)))

(defn- extend-map [map y times]
  (assoc map y (extend-row (nth map y) times)))

(defn- count-trees [start-x start-y delta-x delta-y map]
  (loop [x start-x, y start-y, map map, tree-count 0]
    (let [x          (+ x delta-x)
          y          (+ y delta-y)
          map        (if (>= x (count (nth map y)))
                       (extend-map map y (inc (int (/ x (count (nth map y))))))
                       map)
          tree-count (if (tree? (nth (nth map y) x))
                       (inc tree-count)
                       tree-count)
          ]
      (if
          (< y (dec (count map))) (recur
                                    x
                                    y
                                    map
                                    tree-count)
          tree-count))))

(defn p1 [file]
  (->> file
       slurp
       str/split-lines
       (count-trees 0 0 3 1)))

(p1 file)

(defn p2 [combs file]
  (let [m (->> file slurp str/split-lines)]
    (->> (map (fn [[x y]] (count-trees 0 0 x y m)) combs)
         (reduce *))))

(def combs [[1 1] [3 1] [5 1] [7 1] [1 2]])
(p2 combs file)

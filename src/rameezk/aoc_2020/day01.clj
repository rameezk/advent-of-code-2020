(ns rameezk.aoc-2020.day01)

(defn- find-product-pair [sum-to-find input]
  (set
    (for [x     input
          y     input
          :when (= sum-to-find (+ x y))]
      (* x y))))

(defn p1 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (find-product-pair 2020)))

(defn- find-product-trio [sum-to-find input]
  (set
    (for [x     input
          y     input
          z     input
          :when (= sum-to-find (+ x y z))]
      (* x y z))))

(defn p2 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (find-product-trio 2020)))

(p1 "data/day01.txt")
(p2 "data/day01.txt")

(ns aoc.day01
  (:require [clojure.string :as str]))

                                        ; Loop over numbers to find the pair that sums up to the sum
                                        ; Pair cannot be the same position
(defn- find-product-pair [sum-to-find numbers]
  (let [last-pos (dec (count numbers))]
    (loop [p1 0, p2 1]
      (let [num1 (numbers p1)
            num2 (numbers p2)
            sum  (+ num1 num2)]
        (cond
          (= sum-to-find sum)   (* num1 num2)
          (< p2 last-pos)       (recur p1 (inc p2))
          (< p1 (dec last-pos)) (recur (inc p1) (+ p1 2))
          :else                 "No solution found")))))

(defn p1 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (vec)
       (find-product-pair 2020)))



                                        ; Loop over numbers to find the trio that sums up to the sum
                                        ; Trio cannot be the same position
(defn- find-product-trio [sum-to-find numbers]
  (let [last-pos (dec (count numbers))]
    (loop [p1 0, p2 1, p3 2]
      (let [num1 (numbers p1)
            num2 (numbers p2)
            num3 (numbers p3)
            sum  (+ num1 num2 num3)]
        (cond
          (= sum-to-find sum)   (* num1 num2 num3)
          (< p3 last-pos)       (recur p1 p2 (inc p3))
          (< p2 (dec last-pos)) (recur p1 (inc p2) (+ p2 2))
          (< p1 (- last-pos 2)) (recur (inc p1) (+ p1 2) (+ p1 3))
          :else                 "No solution found")))))

(defn p2 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (vec)
       (find-product-trio 2020)))

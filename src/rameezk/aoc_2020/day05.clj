(ns rameezk.aoc-2020.day05
  (:require [clojure.string :as str]
            [clojure.set :as set]))

; inputs
(def file "data/day05.txt")

(def row-nums (range 0 128))
(def col-nums (range 0 8))

; pvt functions
(defn- calculate [number-range boarding-pass-subset]
  (loop [nr number-range bps boarding-pass-subset]
    (let [c (first bps)
          mid-idx (quot (count nr) 2)
          [lower-half upper-half] (split-at mid-idx nr)]
      (cond
        (pos? (count bps)) (recur (if (or (= c \F) (= c \L)) lower-half upper-half) (rest bps))
        :else nr))))

(defn- get-id [rows cols boarding-pass]
  (let [row-subset (take 7 boarding-pass)
        col-subset (take-last 3 boarding-pass)
        [row] (calculate rows row-subset)
        [col] (calculate cols col-subset)]
    (+ (* row 8) col)))

(defn- missing-seat? [[low high]]
  (when (= high (+ low 2))
    (inc low)))


; main
(defn p1 [file]
  (->> file
       slurp
       str/split-lines
       (map #(get-id rows cols %))
       sort
       last
       ))

(defn p2 [file]
  (->> file
       slurp
       str/split-lines
       (map #(get-id rows cols %))
       sort
       (partition-all 2 1)
       (some missing-seat?)))

(p1 file)
(p2 file)


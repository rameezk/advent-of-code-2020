(ns rameezk.aoc-2020.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input (line-seq (io/reader (io/resource "day07-example.txt"))))
(def real-input (line-seq (io/reader (io/resource "day07.txt"))))

(defn- parse-rule [s]
  (let [[bags & deps] (str/split s #"\s?(contain|,)\s?")
        color         (re-find #"\w+ \w+" bags)]
    [color (keep (comp next (partial re-find #"(\d+) (\w+ \w+)")) deps)]))

;; part 1
(defn- colour-graph [rules]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m col conj bag)) m deps))
          {}
          rules))

(defn- add-valid [result graph colour]
  (into result (get graph colour)))

(defn- valid-outermost [graph start]
  (loop [result (add-valid #{} graph start)]
    (let [result2 (reduce (fn [res colour]
                            (add-valid res graph colour))
                          result result)]
      (if (= result result2)
        result
        (recur result2)))))

(count (valid-outermost (colour-graph (map parse-rule real-input)) "shiny gold"))

;; part 2
(defn- nesting-graph [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m bag conj [(Long/parseLong num) col]))
                    m deps))
          {}
          entries))

(defn- color-count [graph color]
  (let [entries (get graph color)]
    (if (seq entries)
      (reduce
        (fn [cnt [num color]]
          (+ cnt (* num (color-count graph color))))
        1
        entries)
      1)))

(dec (color-count (nesting-graph (map parse-rule real-input)) "shiny gold"))

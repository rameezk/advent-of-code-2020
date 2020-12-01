(ns aoc.day01
  (:require [clojure.string :as str]))

(defn p1 [file]
  (->> file
       (slurp)
       (str/split-lines)))

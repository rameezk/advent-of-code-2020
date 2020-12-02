(ns rameezk.aoc-2020.day02
  (:require [clojure.string :as str]))

(def file "data/day02.txt")

(defn- parse-long [l]
  (Long/parseLong l))

(defn- parse-line [line]
  (let [[_ one two char password]
        (re-find #"(\d+)-(\d+) (.): (.*)" line)]
    [(parse-long one) (parse-long two) (first char) password]))

(defn- password-ok? [[min max char password]]
  (<= min (get (frequencies password) char 0) max))

(defn p1 [file]
  (->> file
       slurp
       str/split-lines
       (map #(parse-line %))
       (filter password-ok?)
       count))

(defn- password-ok-position? [[p1 p2 char password]]
  (let [first-ok  (= char (nth password (dec p1)))
        second-ok (= char (nth password (dec p2)))]
    (not= first-ok second-ok)))

(defn p2 [file]
  (->> file
       slurp
       str/split-lines
       (map #(parse-line %))
       (filter password-ok-position?)
       count))

(p1 file)
(p2 file)

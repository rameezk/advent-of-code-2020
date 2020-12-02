(ns aoc.day02
  (:require [clojure.string :as str]))

(def file "data/day02.txt")

                                        ; Parse the data into a usable structure
(defn- parse-line-min-max-policy [line]
  (let [minmax   (re-seq #"\d+" line)
        min      (first minmax)
        max      (last minmax)
        char     (second (re-find #"([a-z]{1}):" line))
        password (second (re-find #":\s*([a-z].*)" line))]
    (assoc {}
           :min (Integer/parseInt min)
           :max (Integer/parseInt max)
           :char char
           :password password)))

(defn- password-valid-number-occurences? [{min      :min
                                           max      :max
                                           char     :char
                                           password :password}]
  (let [char-count (count (re-seq (re-pattern (str char)) password))]
    (and
      (>= char-count min)
      (<= char-count max))))

(defn p1 [file]
  (->> file
       (slurp)
       (str/split-lines)
       (map #(parse-line-min-max-policy %))
       (map #(password-valid-number-occurences? %))
       (reduce (fn [cnt val]
                 (if val (inc cnt) cnt)) 0)))

(time (p1 file))

                                        ; Parse the data into a usable structure
(defn- parse-line-position-policy [line]
  (let [positions (re-seq #"\d+" line)
        p1        (first positions)
        p2        (last positions)
        char      (second (re-find #"([a-z]{1}):" line))
        password  (second (re-find #":\s*([a-z].*)" line))]
    (assoc {}
           :p1 (dec (Integer/parseInt p1)) ; no 0 index, index starts from 1
           :p2 (dec (Integer/parseInt p2)) ; no 0 index, index starts from 1
           :char char
           :password password)))

(defn- password-valid-position? [{p1       :p1
                                  p2       :p2
                                  char     :char
                                  password :password}]
  (let [char-at-p1 (str (nth password p1))
        char-at-p2 (str (nth password p2))
        valid-p1?  (= char-at-p1 char)
        valid-p2?  (= char-at-p2 char)]
    (= 1 ; exactly only 1 occurence
       (reduce
         (fn [cnt val]
           (if val (inc cnt) cnt))
         0
         [valid-p1? valid-p2?]))))

(defn p2 [file]
  (->> file
       (slurp)
       (str/split-lines)
       (map #(parse-line-position-policy %))
       (map #(password-valid-position? %))
       (reduce (fn [cnt val] (if val (inc cnt) cnt)) 0)
       ))

(time (p2 file))

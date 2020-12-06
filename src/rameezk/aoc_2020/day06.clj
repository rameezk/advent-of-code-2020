(ns rameezk.aoc-2020.day06
  (:require [clojure.string :as str]
            [clojure.set :as set]))


;; inputs
(def file "data/day06.txt")

;; pvts
(defn- str->vec [s]
  (vec s))

(defn- grp->vec [grp]
  (map (fn [i] (str->vec i)) grp))

(defn- grp->set [grp]
  (map (fn [i] (set (str->vec i))) grp))

(defn- count-intersections [sts]
  (if (= 1 (count sts)) (count (first sts))
      (let [intsctn (set/intersection (first sts) (second sts))
            pts     (drop 2 sts)]
        (loop [pts pts, intsctn intsctn]
          (cond
            (pos? (count pts)) (recur
                                 (drop 1 pts)
                                 (set/intersection intsctn (first pts)))
            :else              (count intsctn))))))

;; main
(defn p1 [file]
  (-> file
      slurp
      (str/split #"\R\R")
      (->> (map #(str/split % #"\R"))
           (map #(->> %
                      grp->vec
                      flatten
                      set
                      count))
           (reduce +))))

(defn p2 [file]
  (-> file
      slurp
      (str/split #"\R\R")
      (->> (map #(str/split % #"\R"))
           (map #(->> %
                      grp->set
                      count-intersections))
           (reduce +))))

(p1 file)
(p2 file)

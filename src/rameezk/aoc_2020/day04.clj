(ns rameezk.aoc-2020.day04
  (:require [clojure.set :refer [subset?]]
            [clojure.string :as str]))

(def file "data/day04.txt")

(def rf ["byr"
         "iyr"
         "eyr"
         "hgt"
         "hcl"
         "ecl"
         "pid"])

(defn- split-field [field]
  (let [[f v] (str/split field #":")]
    f))

(defn- valid-passport? [required-fields passport-fields]
  (subset? (set required-fields) (set passport-fields)))

(defn- extract-fields [passport]
  (map #(split-field %) passport))

(defn- count-valid-passports [required-fields passports]
  (reduce (fn [cnt p] (if (valid-passport? required-fields p) (inc cnt) cnt)) 0 passports))

(defn p1 [file]
  (-> file
      slurp
      (str/split #"\n\n")
      (->> (map #(str/split % #"\s|\n"))
           (map #(extract-fields %))
           (count-valid-passports rf))))

(p1 file)

(defn- split-field-and-value [field]
  (let [[f v] (str/split field #":")]
    [f v]))

(defn- extract-fields-and-values [passport]
  (map #(split-field-and-value %) passport))


(defn- valid-passport-fields-and-values? [required-fields passport-fields]
  (and
    (valid-passport? required-fields (map (fn [[f v]] f) passport-fields))
    (every? true? (map (fn [[f v]]  (cond
                                      (= f "byr") (<= 1920 (Integer/parseInt v) 2002)
                                      (= f "iyr") (<= 2010 (Integer/parseInt v) 2020)
                                      (= f "eyr") (<= 2020 (Integer/parseInt v) 2030)
                                      (= f "hgt") (let [[_ m u] (re-find #"(\d+)(.{2})" v)] (cond
                                                                                              (= u "in") (<= 59 (Integer/parseInt m) 76)
                                                                                              (= u "cm") (<= 150 (Integer/parseInt m) 193)
                                                                                              :else      false))
                                      (= f "hcl") (boolean (re-matches #"#[0-9a-f]{6}" v))
                                      (= f "ecl") (boolean (some #{v} ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))
                                      (= f "pid") (boolean (re-matches #"\d{9}" v))
                                      (= f "cid") true
                                      :else       false)) passport-fields))))

(defn- count-valid-passports-with-field-and-value-checks [required-fields passports]
  (reduce
    (fn [cnt p]
      (if (valid-passport-fields-and-values? required-fields p)
        (inc cnt)
        cnt))
    0 passports))

(defn p2 [file]
  (-> file
      slurp
      (str/split #"\n\n")
      (->> (map #(str/split % #"\s|\n"))
           (map #(extract-fields-and-values %))
           (count-valid-passports-with-field-and-value-checks rf))))

(p2 file)

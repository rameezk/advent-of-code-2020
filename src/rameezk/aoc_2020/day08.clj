(ns rameezk.aoc-2020.day08
  (:require [clojure.java.io :as io]))

(def file "data/day08.txt")

(def real-input (->> file slurp))
(def example-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn- parse-input [input]
  (vec (for [[_ instr arg] (re-seq #"(\w{3})\s([\+|-]\d+)" input)]
         [(keyword instr) (read-string arg)])))

(defn- run-boot [input]
  (let [program  (parse-input input)
        init-ctx {:pc    0
                  :acc   0
                  :seen? #{}}]
    (loop [{:keys [pc acc seen?] :as ctx} init-ctx]
      (if (seen? pc)
        acc
        (let [[op arg] (program pc)]
          (case op
            :nop
            (recur (-> ctx
                       (update :pc inc)
                       (update :seen? conj pc)))
            :acc
            (recur (-> ctx
                       (update :acc + arg)
                       (update :pc inc)
                       (update :seen? conj pc)))
            :jmp
            (recur (-> ctx
                       (update :pc + arg)
                       (update :seen conj pc)))))))))

(run-boot real-input)
;; => 1553

(defn- run-boot2 [program]
  (let [init-ctx {:pc    0
                  :acc   0
                  :seen? #{}}]
    (loop [{:keys [pc acc seen?] :as ctx} init-ctx]
      (cond
        (seen? pc)
        :infinite-loop!

        (= pc (count program))
        acc

        :else
        (let [[op arg] (program pc)]
          (case op
            :nop
            (recur (-> ctx
                       (update :pc inc)
                       (update :seen? conj pc)))
            :acc
            (recur (-> ctx
                       (update :acc + arg)
                       (update :pc inc)
                       (update :seen? conj pc)))
            :jmp
            (recur (-> ctx
                       (update :pc + arg)
                       (update :seen conj pc)))))))))

(let [program (parse-input (slurp (io/resource "day08.txt")))]
  (for [i     (range (count program))
        :when (#{:nop :jmp} (get-in program [i 0]))
        :let  [program (update-in program [i 0] {:jmp :nop, :nop :jmp})
               result (run-boot2 program)]
        :when (not= :infinite-loop! result)]
    result))
;; => (1877)


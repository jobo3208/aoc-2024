(ns aoc-2024.day-7
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(def sample-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(def all-operators ['* '+])

(defn parse-line [line]
  (let [numbers (->> (string/split line #":? ")
                     (map #(BigInteger. %)))
        [test-val & operands] numbers]
    [test-val operands]))

(defn parse-input [input]
  (->> (string/split input #"\n")
       (map parse-line)))

(defn apply-operators [operands operators]
  {:pre [(= (count operands) (inc (count operators)))]}
  (let [[first-operand & operands] operands]
    (reduce
      (fn [acc [operator operand]]
        ((resolve operator) acc operand))
      first-operand
      (map vector operators operands))))

(defn get-passing-operator-seqs [equation]
  (let [[test-val operands] equation
        num-operators (dec (count operands))
        operator-seqs (combo/selections all-operators num-operators)]
    (filter #(= (apply-operators operands %) test-val) operator-seqs)))

(defn solve-1 [input]
  (let [equations (parse-input input)
        solvable (filter (comp first get-passing-operator-seqs) equations)]
    (reduce + (map first solvable))))

(defn || [x y]
  (BigInteger. (str x y)))

(defn solve-2 [input]
  (with-redefs [all-operators ['* '+ '||]]
    (solve-1 input)))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day7.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day7.txt"))))

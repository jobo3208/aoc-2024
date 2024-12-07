(ns aoc-2024.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "3   4
4   3
2   5
1   3
3   9
3   3")

(defn parse-line [line]
  (->> (string/split line #" +")
       (mapv #(Integer. %))))

(defn transpose [m]
  (apply mapv vector m))

(defn solve-1 [input]
  (let [[list1 list2]
        (->> (string/split input #"\n")
             (map parse-line)
             (transpose))
        diffs (map #(abs (- %1 %2)) (sort list1) (sort list2))]
    (reduce + diffs)))

(defn solve-2 [input]
  (let [[list1 list2]
        (->> (string/split input #"\n")
             (map parse-line)
             (transpose))
        l2-freqs (frequencies list2)
        scores (map #(* % (get l2-freqs % 0)) list1)]
    (reduce + scores)))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day1.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day1.txt"))))

(ns aoc-2024.day-3
  (:require [clojure.java.io :as io]))

(def sample-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn solve-1 [input]
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input)
       (map (fn [[_ a b]] (* (Integer. a) (Integer. b))))
       (reduce +)))

(def sample-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn solve-2 [input]
  (reduce
    (fn [[do? total] [s a b]]
      (cond
        (= s "don't()") [false total]
        (= s "do()") [true total]
        do? [true (+ total (* (Integer. a) (Integer. b)))]
        :else [false total]))
    [true 0]
    (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)" input)))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day3.txt")))

  (solve-2 sample-input-2)
  (solve-2 (slurp (io/resource "day3.txt"))))

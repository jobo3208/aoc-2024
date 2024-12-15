(ns aoc-2024.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "125 17")

(defn parse-input [input]
  (map #(BigInteger. %) (string/split input #"[ \n]")))

(defn change [stone]
  (cond
    (zero? stone) (list 1)
    (even? (count (str stone)))
    (let [stone-str (str stone)
          num-digits (count stone-str)]
      (list
        (BigInteger. (subs stone-str 0 (/ num-digits 2)))
        (BigInteger. (subs stone-str (/ num-digits 2)))))
    :else (list (* stone 2024))))

(defn blink [stones]
  (mapcat change stones))

(defn solve-1 [input]
  (let [stones (parse-input input)]
    (count (nth (iterate blink stones) 25))))

(def num-stones-after
  (memoize
    (fn [stone num-iterations]
      (if (zero? num-iterations)
        1
        (let [stones' (blink (list stone))]
          (reduce + (map #(num-stones-after % (dec num-iterations)) stones')))))))

(defn solve-2 [input]
  (let [stones (parse-input input)]
    (reduce + (map #(num-stones-after % 75) stones))))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day11.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day11.txt"))))

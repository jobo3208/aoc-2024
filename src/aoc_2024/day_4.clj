(ns aoc-2024.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defn transpose [m]
  (apply mapv vector m))

(defn rows [grid]
  grid)

(defn cols [grid]
  (transpose grid))

(defn v+ [v1 v2]
  (mapv + v1 v2))

(defn in-bounds? [h w [y x]]
  (and (<= 0 y (dec w))
       (<= 0 x (dec h))))

(defn diag [grid step from]
  (let [h (count grid)
        w (count (first grid))
        in-bounds? (partial in-bounds? h w)
        coords (take-while in-bounds? (iterate (partial v+ step) from))]
    (mapv (partial get-in grid) coords)))

(defn se-diags [grid]
  (let [h (count grid)
        w (count (first grid))
        se-diag-from (partial diag grid [1 1])]
    (mapv se-diag-from (concat (for [y (range h)] [y 0])
                               (for [x (range 1 w)] [0 x])))))

(defn ne-diags [grid]
  (let [h (count grid)
        w (count (first grid))
        ne-diag-from (partial diag grid [-1 1])]
    (mapv ne-diag-from (concat (for [y (range h)] [y 0])
                               (for [x (range 1 w)] [(dec h) x])))))

(defn solve-1 [input]
  (let [grid (mapv vec (string/split input #"\n"))
        forward-seqs (mapcat #(% grid) [rows cols se-diags ne-diags])
        all-seqs (concat forward-seqs (map reverse forward-seqs))
        all-strs (map (partial apply str) all-seqs)
        matches (mapcat (partial re-seq #"XMAS") all-strs)]
    (count matches)))

(defn x-mas? [grid [y x]]
  (and (= (get-in grid [y x]) \A)
       (let [nw (get-in grid [(dec y) (dec x)])
             sw (get-in grid [(inc y) (dec x)])
             ne (get-in grid [(dec y) (inc x)])
             se (get-in grid [(inc y) (inc x)])]
         (and (= (set [nw se]) #{\M \S})
              (= (set [sw ne]) #{\M \S})))))

(defn solve-2 [input]
  (let [grid (mapv vec (string/split input #"\n"))
        h (count grid)
        w (count (first grid))]
    (count
      (filter (partial x-mas? grid)
              (for [y (range h)
                    x (range w)]
                [y x])))))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day4.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day4.txt"))))

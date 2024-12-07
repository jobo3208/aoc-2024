(ns aoc-2024.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse-line [line]
  (->> (string/split line #" +")
       (mapv #(Integer. %))))

(defn get-diffs [report]
  (let [pairs (partition 2 1 report)]
    (map (fn [[x y]] (- y x)) pairs)))

(defn safe? [diffs]
  (and (or (every? pos? diffs)
           (every? neg? diffs))
       (every? #(<= (abs %) 3) diffs)))

(defn solve-1 [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (map get-diffs)
       (filter safe?)
       (count)))

(defn sign [n]
  (if (pos? n) :pos :neg))

(defn illegal-diff? [expected-sign n]
  (or (not= (sign n) expected-sign)
      (< (abs n) 1)
      (> (abs n) 3)))

(defn get-illegal-diff-indexes [diffs]
  (let [; Dominant sign = most common sign amongst first 5 diffs.
        ; The opposite sign will be considered illegal.
        dominant-sign (->> (take 5 diffs)
                           (map sign)
                           (frequencies)
                           (sort-by val)
                           (last)
                           (key))]
    (keep-indexed (fn [i d]
                    (when (illegal-diff? dominant-sign d)
                      i))
                  diffs)))

(defn safe-after-removing? [report i]
  (let [report' (into (subvec report 0 i) (subvec report (inc i)))
        diffs' (get-diffs report')]
    (safe? diffs')))

(defn safe-with-problem-dampener? [report]
  (let [diffs (get-diffs report)]
    (if (safe? diffs)
      true
      (let [illegal-diff-idxs (get-illegal-diff-indexes diffs)
            ; If the diff at i is illegal, then the levels at i and i + 1 might
            ; be problematic.
            suspect-level-idxs (mapcat #(-> [% (inc %)]) illegal-diff-idxs)]
        (some (partial safe-after-removing? report) suspect-level-idxs)))))

(defn solve-2 [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (filter safe-with-problem-dampener?)
       (count)))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day2.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day2.txt"))))

(ns aoc-2024.day-8
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(def sample-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn parse-input [input]
  (let [m (->> (string/split input #"\n")
               (mapv vec))
        h (count m)
        w (count (first m))
        ant-map (reduce
                  (fn [ant-map [y x]]
                    (let [c (get-in m [y x])]
                      (if (= c \.)
                        ant-map
                        (update ant-map c #(conj % [y x])))))
                  {}
                  (for [y (range h), x (range w)] [y x]))]
    [ant-map h w]))

(defn get-antinodes [p1 p2]
  (let [delta (mapv - p2 p1)]
    [(mapv - p1 delta)
     (mapv + p2 delta)]))

(defn in-bounds? [[h w] [y x]]
  (and (<= 0 y (dec h))
       (<= 0 x (dec w))))

(defn solve-1 [input]
  (let [[ant-map h w] (parse-input input)
        antinodes (mapcat (fn [ants]
                            (->> (combo/combinations ants 2)
                                 (mapcat (partial apply get-antinodes))))
                          (vals ant-map))]
    (->> antinodes
         (filter (partial in-bounds? [h w]))
         (into #{})
         (count))))

(defn get-antinodes-with-resonant-harmonics [[h w] p1 p2]
  (let [delta (mapv - p2 p1)]
    (concat
      (take-while (partial in-bounds? [h w]) (iterate #(mapv - % delta) p1))
      (take-while (partial in-bounds? [h w]) (iterate #(mapv + % delta) p2)))))

(defn solve-2 [input]
  (let [[ant-map h w] (parse-input input)
        antinodes (mapcat (fn [ants]
                            (->> (combo/combinations ants 2)
                                 (mapcat (partial apply get-antinodes-with-resonant-harmonics [h w]))))
                          (vals ant-map))]
    (->> antinodes
         (into #{})
         (count))))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day8.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day8.txt"))))

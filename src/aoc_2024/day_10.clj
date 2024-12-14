(ns aoc-2024.day-10
  (:require [clojure.java.io :as io]
            [clojure.set :as se]
            [clojure.string :as string]))

(def sample-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn parse-input [input]
  (->> (string/split input #"\n")
       (mapv (partial mapv #(Character/digit % 10)))))

(defn get-dimensions [top-map]
  [(count top-map) (count (first top-map))])

(defn get-trailheads [top-map]
  (let [[h w] (get-dimensions top-map)]
    (->> (for [y (range h), x (range w)] [y x])
         (filter #(zero? (get-in top-map %))))))

(defn in-bounds? [[h w] [y x]]
  (and (<= 0 y (dec h))
       (<= 0 x (dec w))))

(defn get-neighbors [[h w] [y x]]
  (let [poss-neighbors [[y (dec x)]
                        [(dec y) x]
                        [y (inc x)]
                        [(inc y) x]]]
    (filter (partial in-bounds? [h w]) poss-neighbors)))

(defn extend-poss-trail [top-map poss-trail]
  (let [[h w] (get-dimensions top-map)
        end (peek poss-trail)
        end-height (get-in top-map end)
        neighbors (get-neighbors [h w] end)
        valid-neighbors (filter #(= (get-in top-map %) (inc end-height)) neighbors)]
    (map #(conj poss-trail %) valid-neighbors)))

(defn is-trail? [top-map poss-trail]
  (let [end (peek poss-trail)]
    (= (get-in top-map end) 9)))

(defn get-trails [top-map]
  (let [trailheads (get-trailheads top-map)]
    (loop [poss-trails (set (map vector trailheads))
           trails #{}]
      (if (seq poss-trails)
        (let [poss-trails' (set (mapcat (partial extend-poss-trail top-map) poss-trails))
              new-trails (set (filter (partial is-trail? top-map) poss-trails'))]
          (recur (se/difference poss-trails' new-trails)
                 (se/union trails new-trails)))
        trails))))

(defn solve-1 [input]
  (let [top-map (parse-input input)
        trails (get-trails top-map)
        scores (->> (map (juxt first peek) trails)
                    (into #{})
                    (map first)
                    (frequencies))]
    (reduce + (vals scores))))

(defn solve-2 [input]
  (let [top-map (parse-input input)
        trails (get-trails top-map)
        ratings (->> (map first trails)
                     (frequencies))]
    (reduce + (vals ratings))))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day10.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day10.txt"))))

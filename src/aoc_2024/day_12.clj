(ns aoc-2024.day-12
  (:require [clojure.java.io :as io]
            [clojure.set :as se]
            [clojure.string :as string]))

(def sample-input "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(defn parse-input [input]
  (mapv vec (string/split input #"\n")))

(defn get-dimensions [m]
  [(count m) (count (first m))])

(defn in-bounds? [[h w] [y x]]
  (and (<= 0 y (dec h))
       (<= 0 x (dec w))))

(defn get-neighbors [[h w] [y x]]
  (let [poss-neighbors [[y (dec x)]
                        [(dec y) x]
                        [y (inc x)]
                        [(inc y) x]]]
    (filter (partial in-bounds? [h w]) poss-neighbors)))

(defn get-neighbors-in-region [[h w] [y x] m]
  (->> (get-neighbors [h w] [y x])
       (filter #(= (get-in m [y x]) (get-in m %)))))

(defn find-regions [m]
  (let [[h w] (get-dimensions m)]
    (loop [unassigned (set (for [y (range h), x (range w)] [y x]))
           cur-region nil
           regions #{}
           frontier #{}]
      (cond
        (and (empty? unassigned) (nil? cur-region))
        regions

        (nil? cur-region)
        (let [start (first unassigned)]
          (recur (disj unassigned start)
                 {:letter (get-in m start) :points #{}}
                 regions
                 #{start}))

        (empty? frontier)
        (recur unassigned nil (conj regions cur-region) #{})

        :else
        (let [unassigned' (se/difference unassigned frontier)
              cur-region' (update cur-region :points #(se/union % frontier))
              neighbors-in-region (into #{} (mapcat #(get-neighbors-in-region [h w] % m) frontier))
              frontier' (se/difference neighbors-in-region frontier (:points cur-region))]
          (recur unassigned' cur-region' regions frontier'))))))

(defn get-area [region]
  (count (:points region)))

(defn get-perimeter-of-point [m region p]
  (let [[h w] (get-dimensions m)
        neighbors (get-neighbors [h w] p)]
    (- 4 (count (se/intersection (set neighbors) (:points region))))))

(defn get-perimeter [m region]
  (let [points (:points region)
        pt-perimeters (map (partial get-perimeter-of-point m region) points)]
    (reduce + pt-perimeters)))

(defn solve-1 [input]
  (let [m (parse-input input)
        regions (find-regions m)]
    (->> (map * (map get-area regions) (map (partial get-perimeter m) regions))
         (reduce +))))

(defn get-neighbors-with-direction [[y x]]
  {:w [y (dec x)]
   :n [(dec y) x]
   :e [y (inc x)]
   :s [(inc y) x]})

(defn get-boundaries-of-point [m p]
  (->> (get-neighbors-with-direction p)
       (remove (fn [[_ q]] (= (get-in m p) (get-in m q))))
       (map (fn [[dir _]] [p dir]))))

(defn get-num-sides [m region]
  (let [boundaries (mapcat (partial get-boundaries-of-point m) (:points region))]
    (first (reduce
            (fn [[i lst] [dir a b]]
              (if (= lst [dir a (dec b)])
                [i [dir a b]]
                [(inc i) [dir a b]]))
            [0 nil]
            (sort (map (fn [[[y x] dir]]
                         (case dir
                           (:w :e) [dir x y]
                           (:n :s) [dir y x]))
                      boundaries))))))

(defn solve-2 [input]
  (let [m (parse-input input)
        regions (find-regions m)]
    (->> (map * (map get-area regions) (map (partial get-num-sides m) regions))
         (reduce +))))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day12.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day12.txt"))))

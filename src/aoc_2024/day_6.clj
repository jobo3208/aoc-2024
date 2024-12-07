(ns aoc-2024.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(def dir->delta
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

(def turn
  {:north :east
   :east :south
   :south :west
   :west :north})

(defn parse-input [input]
  (let [m (->> (string/split input #"\n")
               (mapv vec))
        h (count m)
        w (count (first m))
        points (for [y (range h), x (range w)] [y x])]
    (reduce
      (fn [room-map [y x]]
        (let [c (get-in m [y x])]
          (case c
            \# (update room-map :obstacles #(conj % [y x]))
            \^ (assoc room-map :guard-start [[y x] :north])
            room-map)))
      {:dimensions [h w]
       :obstacles #{}}
      points)))

(defn in-bounds? [[h w] [y x]]
  (and (<= 0 y (dec h))
       (<= 0 x (dec w))))

(defn step [room-map [[y x] facing]]
  (let [{:keys [obstacles]} room-map
        [y' x'] (mapv + [y x] (dir->delta facing))]
    (if (obstacles [y' x'])
      [[y x] (turn facing)]
      [[y' x'] facing])))

(defn simulate [room-map]
  (let [{:keys [dimensions]} room-map]
    (loop [[[y x] facing] (:guard-start room-map)
           occupied #{}]
      (if (in-bounds? dimensions [y x])
        (recur (step room-map [[y x] facing]) (conj occupied [y x]))
        occupied))))

(defn solve-1 [input]
  (let [room-map (parse-input input)
        occupied (simulate room-map)]
    (count occupied)))

(defn simulate-with-loop-check [room-map]
  (let [{:keys [dimensions]} room-map]
    (loop [[[y x] facing] (:guard-start room-map)
           occupied #{}
           trail #{}] ; like occupied, but includes direction
      (cond
        (trail [[y x] facing]) :loop
        (in-bounds? dimensions [y x]) (recur (step room-map [[y x] facing])
                                             (conj occupied [y x])
                                             (conj trail [[y x] facing]))
        :else occupied))))

(defn solve-2 [input]
  (let [room-map (parse-input input)
        occupied (simulate-with-loop-check room-map)
        guard-start-point (-> room-map :guard-start first)
        possible-obstacle-points (disj occupied guard-start-point)
        possible-room-maps (map (fn [[y x]]
                                  (update room-map :obstacles #(conj % [y x])))
                                possible-obstacle-points)
        loops (filter #(= (simulate-with-loop-check %) :loop) possible-room-maps)]
    (count loops)))

(comment
  (assert (= (parse-input sample-input)
             {:dimensions [10 10]
              :obstacles #{[0 4] [1 9] [3 2] [4 7] [6 1] [7 8] [8 0] [9 6]}
              :guard-start [[6 4] :north]}))

  (let [room-map (parse-input sample-input)]
    (step room-map [[1 4] :north]))

  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day6.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day6.txt"))))

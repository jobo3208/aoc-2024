(ns aoc-2024.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defn parse-machine-config-str [machine-config-str]
  (let [nums (->> (re-seq #"\d+" machine-config-str)
                  (map #(BigInteger. %)))
        [ax ay bx by px py] nums]
    {:a [ax ay] :b [bx by] :prize [px py]}))

(defn parse-input [input]
  (let [machine-config-strs (string/split input #"\n\n")]
    (map parse-machine-config-str machine-config-strs)))

(defn wins? [machine-config presses]
  (let [{:keys [a b prize]} machine-config
        [ax ay] a
        [bx by] b
        [pa pb] presses]
    (= prize (mapv + [(* pa ax) (* pa ay)] [(* pb bx) (* pb by)]))))

(defn get-cost [presses]
  (let [[pa pb] presses]
    (+ (* 3 pa) pb)))

(defn get-cheapest-win [machine-config]
  (->> (for [pa (range 100), pb (range 100)] [pa pb])
       (filter (partial wins? machine-config))
       (sort-by get-cost)
       (first)))

(defn solve-1 [input]
  (let [machine-configs (parse-input input)
        costs (map #(if-some [presses (get-cheapest-win %)]
                      (get-cost presses)
                      0)
                   machine-configs)]
    (reduce + costs)))

(defn solve-eqns [machine-config]
  ; ...pen and paper...
  (let [{:keys [a b prize]} machine-config
        [[ax ay] [bx by] [px py]] [a b prize]
        b (/ (- (* ax py) (* ay px))
             (- (* ax by) (* ay bx)))
        a (/ (- px (* bx b)) ax)]
    [a b]))

(defn valid? [[pa pb]]
  (every? integer? [pa pb]))

(defn solve-2 [input]
  (let [machine-configs
        (->> (parse-input input)
             (map (fn [mc]
                    (update mc :prize #(mapv (partial + 10000000000000) %)))))]
    (->> machine-configs
         (map solve-eqns)
         (filter valid?)
         (map get-cost)
         (reduce +))))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day13.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day13.txt"))))

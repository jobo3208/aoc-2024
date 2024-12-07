(ns aoc-2024.day-5
  (:require [clojure.java.io :as io]
            [clojure.set :as se]
            [clojure.string :as string]))

(def sample-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse-rule [line]
  (->> (string/split line #"\|")
       (mapv #(Integer. %))))

(defn parse-update [line]
  (->> (string/split line #",")
       (mapv #(Integer. %))))

(defn parse-input [input]
  (let [[rules-str updates-str](string/split input #"\n\n")
        rules (->> (string/split rules-str #"\n")
                   (map parse-rule))
        updates (->> (string/split updates-str #"\n")
                     (map parse-update))]
    [rules updates]))

(defn obeys? [upd rule]
  (let [occurrences (filterv (set rule) upd)]
    (if (= (count occurrences) 2)
      (= occurrences rule)
      true)))

(defn correct? [rules upd]
  (every? (partial obeys? upd) rules))

(defn get-pertinent-rules [rules upd]
  (let [upd-set (set upd)]
    (filter (fn [[a b]]
              (and (upd-set a) (upd-set b)))
            rules)))

(defn fix [rules upd]
  (let [pertinent-rules (get-pertinent-rules rules upd)]
    (loop [fixed []
           upd-set (set upd)
           rules pertinent-rules]
      (if (seq rules)
        (let [start-page (se/difference (set (map first rules))
                                        (set (map second rules)))
              _ (assert (= (count start-page) 1))
              start-page (first start-page)
              fixed (conj fixed start-page)
              upd-set (disj upd-set start-page)
              rules (remove #(= (first %) start-page) rules)]
          (recur fixed upd-set rules))
        (conj fixed (first upd-set))))))

(defn solve-1 [input]
  (let [[rules updates] (parse-input input)
        correct-updates (filter (partial correct? rules) updates)
        middle-numbers (map (fn [upd]
                              (nth upd (quot (count upd) 2)))
                            correct-updates)]
    (reduce + middle-numbers)))

(defn solve-2 [input]
  (let [[rules updates] (parse-input input)
        incorrect-updates (remove (partial correct? rules) updates)
        fixed-updates (map (partial fix rules) incorrect-updates)
        middle-numbers (map (fn [upd]
                              (nth upd (quot (count upd) 2)))
                            fixed-updates)]
    (reduce + middle-numbers)))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day5.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day5.txt"))))

(ns aoc-2024.day-9
  (:require [clojure.java.io :as io]))

(def sample-input "2333133121414131402")

(defn parse-input [input]
  (->> (mapcat (fn [i n]
                 (let [n (Character/digit n 10)
                       v (if (even? i) (/ i 2) nil)]
                   (repeat n v)))
               (range 0 (count input))
               input)
       (into [])))

(defn frag [blocks]
  (loop [bs blocks
         i 0
         j (dec (count blocks))]
    (let [bi (get bs i)
          bj (get bs j)]
      (cond
        (= i j) bs
        (some? bi) (recur bs (inc i) j)
        (nil? bj) (recur bs i (dec j))
        :else (recur (assoc bs i bj, j bi) (inc i) (dec j))))))

(defn checksum [blocks]
  (->> (keep-indexed (fn [i b]
                       (when b
                         (* i b)))
                     blocks)
       (reduce +)))

(defn solve-1 [input]
  (let [blocks (parse-input input)
        fragged (frag blocks)]
    (checksum fragged)))

(comment
  ; false start
  (defn parse-input-2 [input]
    (->> (map-indexed (fn [i n]
                        (let [n (Character/digit n 10)]
                          (if (even? i)
                            [:file (/ i 2) n]
                            [:free n])))
                      input)
         (into [])))

  (defn move [regions from-idx to-idx]
    (let [[from-type from-id from-length :as from] (get regions from-idx)
          [to-type to-length :as to] (get regions to-idx)]
      (assert (= from-type :file))
      (assert (= to-type :free))
      (let [from' [[:free from-length]]
            to' (if (> to-length from-length)
                  [from [:free (- to-length from-length)]]
                  [from])]
        (into [] (concat (subvec regions 0 to-idx)
                         to'
                         (subvec regions (inc to-idx) from-idx)
                         from'
                         (subvec regions (inc from-idx)))))))

  (defn solve-2 [input]
    (let [regions (parse-input-2 input)]
      (move regions 18 1))))

(defn parse-input-into-files [input]
  (->> (partition 2 2 "0" input)
       (map-indexed (fn [i [file-len free-len]]
                      (let [file-len (Character/digit file-len 10)
                            free-len (Character/digit free-len 10)]
                        {:id i, :len file-len, :pad free-len})))
       (into [])))

(defn remove-file [files idx]
  (let [file (get files idx)
        lhs (get files (dec idx))
        lhs' (update lhs :pad (partial + (:len file) (:pad file)))
        files (into [] (concat
                         (subvec files 0 (dec idx))
                         [lhs']
                         (subvec files (inc idx))))]
    files))

(defn add-file [files idx file]
  (let [dest (get files idx)
        dest' (assoc dest :pad 0)
        dest-rhs' (assoc file :pad (- (:pad dest) (:len file)))
        files (into [] (concat
                         (subvec files 0 idx)
                         [dest' dest-rhs']
                         (subvec files (inc idx))))]
    files))

(defn move-file [files from-idx to-idx]
  (-> files
      (remove-file from-idx)
      (add-file to-idx (get files from-idx))))

(comment
  (assert (= (move-file [{:id 0 :len 3 :pad 3} {:id 1 :len 2 :pad 2}] 1 0)
             [{:id 0 :len 3 :pad 0} {:id 1 :len 2 :pad 5}]))

  (assert (= (move-file [{:id 0 :len 3 :pad 3} {:id 1 :len 2 :pad 2} {:id 2 :len 1 :pad 1}] 2 0)
             [{:id 0 :len 3 :pad 0} {:id 2 :len 1 :pad 2} {:id 1 :len 2 :pad 4}])))

(defn move-all-files [files]
  (loop [files files
         file-id (apply max (map :id files))]
    (if (nat-int? file-id)
      (let [[from-idx from] (first (keep-indexed (fn [i file]
                                                   (when (= (:id file) file-id)
                                                     [i file]))
                                                 files))
            [to-idx _] (first (keep-indexed (fn [i file]
                                              (when (and (< i from-idx)
                                                         (>= (:pad file) (:len from)))
                                                [i file]))
                                            files))
            files' (if (some? to-idx)
                     (move-file files from-idx to-idx)
                     files)]
        (recur files' (dec file-id)))
      files)))

(defn files->blocks [files]
  (->> (mapcat (fn [file]
                 (concat
                   (repeat (:len file) (:id file))
                   (repeat (:pad file) nil)))
               files)
       (into [])))

(defn solve-2 [input]
  (let [files (parse-input-into-files input)
        files' (move-all-files files)
        blocks' (files->blocks files')]
    (checksum blocks')))

(comment
  (solve-1 sample-input)
  (solve-1 (slurp (io/resource "day9.txt")))

  (solve-2 sample-input)
  (solve-2 (slurp (io/resource "day9.txt"))))

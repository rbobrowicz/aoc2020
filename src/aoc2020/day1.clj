(ns aoc2020.day1
  (:require [clojure.string :as str]))

(defn read-input-file [path]
  (->> (slurp path)
       (str/split-lines)
       (map #(Integer/parseInt %))))

(defn find-sum [target coll]
  (let [coll-as-set (set coll)]
    (first
     (for [x coll
           :let [y (- target x)]
           :when (get coll-as-set y)]
       [x y]))))

(defn find-triple-sum [target coll]
  (let [coll-as-set (set coll)]
    (first
     (for [x coll
           :let [y (- target x)
                 answer (find-sum y coll-as-set)]
           :when answer]
       (conj answer x)))))

(defn problem-1 []
  (let [data (read-input-file "./resources/day1.input")]
    (apply * (find-sum 2020 data))))

(defn problem-2 []
  (let [data (read-input-file "./resources/day1.input")]
    (apply * (find-triple-sum 2020 data))))

(comment
  (problem-1)
  (problem-2))

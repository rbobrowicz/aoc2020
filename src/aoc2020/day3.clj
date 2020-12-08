(ns aoc2020.day3
  (:require [clojure.string :as str]))

(defn read-input-file [path]
  (->> (slurp path)
       (str/split-lines)))

(defn trace-path [terrain x-slope y-slope]
  (loop [x 0
         y-lines terrain
         path (transient [])]
    (if-let [y-line (first y-lines)]
      (recur (+ x x-slope)
             (nthrest y-lines y-slope)
             (conj! path (get y-line (mod x (count y-line)))))
      (persistent! path))))

(defn count-trees [path]
  (->> path
       (filter #(= \# %))
       count))

(defn problem-1 []
  (let [data (read-input-file "./resources/day3.input")]
    (count-trees (trace-path data 3 1))))

(defn problem-2 []
  (let [data (read-input-file "./resources/day3.input")
        r1d1 (count-trees (trace-path data 1 1))
        r3d1 (count-trees (trace-path data 3 1))
        r5d1 (count-trees (trace-path data 5 1))
        r7d1 (count-trees (trace-path data 7 1))
        r1d2 (count-trees (trace-path data 1 2))]
    (* r1d1 r3d1 r5d1 r7d1 r1d2)))

(comment
  (problem-1)
  (problem-2))

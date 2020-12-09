(ns aoc2020.day9
  (:require [clojure.string :as str]))

(defn read-input-file [path]
  (->> (slurp path)
       str/split-lines
       (map #(Long/parseLong %))))

(defn target-sums [xs target]
  (for [x xs
        y xs
        :when (and (not= x y)
                   (= target (+ x y)))]
    [x y]))

(defn find-not-sum [data]
  (loop [prev-25 (into clojure.lang.PersistentQueue/EMPTY
                       (take 25 data))
         data (drop 25 data)]
    (when-let [n (first data)]
      (if (seq (target-sums prev-25 n))
        (recur (pop (conj prev-25 n))
               (rest data))
        n))))

(defn add-to-target [xs target]
  (loop [xs xs
         total 0
         items (transient [])]
    (when-let [x (first xs)]
      (cond
        (= total target) (persistent! items)
        (< total target) (recur (rest xs)
                                (+ total x)
                                (conj! items x))
        :else nil))))

(defn find-weakness [data target]
  (loop [xs data]
    (when-let [s (seq xs)]
      (if-let [items (add-to-target s target)]
        (+ (apply max items) (apply min items))
        (recur (rest s))))))

(defn problem-1 []
  (let [data (read-input-file "./resources/day9.input")]
    (find-not-sum data)))

(defn problem-2 []
  (let [data (read-input-file "./resources/day9.input")]
    (find-weakness data
                   (find-not-sum data))))

(comment
  (problem-1)
  (problem-2))

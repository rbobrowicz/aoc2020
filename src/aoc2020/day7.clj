(ns aoc2020.day7
  (:require [clojure.string :as str]))

(def bag-regex #"([a-z]+) ([a-z]+) bags contain")
(def contain-regex #"(\d+) ([a-z]+) ([a-z]+) bag")

(defn bag->keyword [bag]
  (keyword (apply str (take-last 2 bag))))

(defn parse-inner-bag [bag]
  {:count (Integer/parseInt (get bag 1))
   :bag (bag->keyword bag)})

(defn parse-line [line]
  (let [outer-bag (re-find bag-regex line)
        inner-bags (re-seq contain-regex line)]
    [(bag->keyword outer-bag) (map parse-inner-bag inner-bags)]))

(defn read-input-file [path]
  (->> (slurp path)
       str/split-lines
       (map parse-line)
       (into {})))

(defn add-edge [graph src dst]
  (update graph src #(if (nil? %1) [%2] (conj %1 %2)) dst))

(defn invert-graph [rules]
  (loop [bags rules
         graph {}]
    (if-let [[bag containing] (first bags)]
      (recur (rest bags)
             (reduce #(add-edge %1 %2 bag) graph (map :bag containing)))
      graph)))

(defn reachable-nodes [graph origin]
  (loop [queue [origin]
         reachables #{}]
    (if-let [curr-node (peek queue)]
      (let [destinations (get graph curr-node)]
        (recur (into (pop queue) destinations)
               (into reachables destinations)))
      reachables)))

(defn number-of-bags [rules bag]
  (apply + 1
         (map #(* (:count %1)
                  (number-of-bags rules (:bag %1)))
              (get rules bag))))

(defn problem-1 []
  (let [rules (read-input-file "./resources/day7.input")]
    (count
     (reachable-nodes (invert-graph rules)
                      :shinygold))))

(defn problem-2 []
  (let [rules (read-input-file "./resources/day7.input")]
    (dec (number-of-bags rules :shinygold))))

(comment
  (problem-1)
  (problem-2))

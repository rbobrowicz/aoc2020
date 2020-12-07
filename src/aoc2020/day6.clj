(ns aoc2020.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input-file [path]
  (as->
      (slurp path) $
      (str/split $ #"\n\n")
      (map str/split-lines $)))

(defn number-of-yes [group]
  (count (into #{} (str/join group))))

(defn number-of-all-yes [group]
  (->> group
       (map #(into #{} %))
       (apply set/intersection)
       count))

(defn problem-1 []
  (let [data (read-input-file "./resources/day6.input")]
    (apply + (map number-of-yes data))))

(defn problem-2 []
  (let [data (read-input-file "./resources/day6.input")]
    (apply + (map number-of-all-yes data))))

(comment
  (problem-1)
  (problem-2))

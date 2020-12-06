(ns aoc2020.day2
  (:require [clojure.string :as str]))

(def line-regex #"\s*(\d+)-(\d+)\s+(\D):\s+(.*)")
(defn parse-line [line]
  (let [parsed (re-find line-regex line)]
    {:line (get parsed 0)
     :low (-> (get parsed 1) Integer/parseInt)
     :high (-> (get parsed 2) Integer/parseInt)
     :letter (-> (get parsed 3) first)
     :pass (get parsed 4)}))

(defn read-input-file [path]
  (->> (slurp path)
       (str/split-lines)
       (map parse-line)))

(defn is-valid? [password]
  (let [only-target (filter #(= (:letter password) %)
                            (:pass password))
        num-occurances (count only-target)]
    (<= (:low password) num-occurances (:high password))))

(defn is-valid-2? [password]
  (let [pos1 (-> password :pass (get (dec (:low password))))
        pos2 (-> password :pass (get (dec (:high password))))
        pos1-is-target (= pos1 (:letter password))
        pos2-is-target (= pos2 (:letter password))]
    (or (and pos1-is-target (not pos2-is-target))
        (and (not pos1-is-target) pos2-is-target))))

(defn problem-1 []
  (let [data (read-input-file "./resources/day2.input")]
    (count (filter is-valid? data))))

(defn problem-2 []
  (let [data (read-input-file "./resources/day2.input")]
    (count (filter is-valid-2? data))))

(comment
  (problem-1)
  (problem-2))

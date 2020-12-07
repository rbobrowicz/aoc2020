(ns aoc2020.day5
  (:require [clojure.string :as str]))

(defn read-input-file [path]
  (->> (slurp path)
       (str/split-lines)))

(defn split-range [[low high] step]
  (let [midpoint (+ low (quot (- high low) 2))]
    (if (= step :low)
      [low midpoint]
      [(inc midpoint) high])))

(defn narrow [range path]
  (-> (reductions split-range range path)
      last
      first))

(defn narrow-rows [path] (narrow [0 127] path))
(defn narrow-columns [path] (narrow [0 7] path))

(def input-map
  {\F :low
   \B :high
   \L :low
   \R :high})

(defn get-seat-coordinate [pass]
  (let [full-path (map #(get input-map %) pass)
        [row-path column-path] (split-at 7 full-path)]
    [(narrow-rows row-path) (narrow-columns column-path)]))

(defn seat-id [[row column]]
  (+ column (* 8 row)))

(defn pass->id [pass]
  (-> pass
      get-seat-coordinate
      seat-id))

(defn find-seat [seats]
  (let [seats-as-set (set seats)]
    (first
     (for [candidate (range 0 1028)
           :when (and
                  (not (contains? seats-as-set candidate))
                  (contains? seats-as-set (dec candidate))
                  (contains? seats-as-set (inc candidate)))]
       candidate))))

(defn problem-1 []
  (let [data (read-input-file "./resources/day5.input")
        seat-ids (map pass->id data)]
    (apply max seat-ids)))

(defn problem-2 []
  (let [data (read-input-file "./resources/day5.input")
        seat-ids (map pass->id data)]
    (find-seat seat-ids)))

(comment
  (problem-1)
  (problem-2))

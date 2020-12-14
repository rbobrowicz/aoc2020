(ns aoc2020.day10
  (:require [clojure.string :as str]))

(defn read-input-file [path]
  (->> (slurp path)
       str/split-lines
       (map #(Integer/parseInt %))))

(defn get-device-joltage [data]
  (+ 3 (reduce max data)))

(defn count-joltage-jumps [data]
  (loop [one-jumps 0
         two-jumps 0
         three-jumps 0
         data (partition 2 1 (sort data))]
    (if-let [[x y] (first data)]
      (case (- y x)
        1 (recur (inc one-jumps) two-jumps three-jumps (rest data))
        2 (recur one-jumps (inc two-jumps) three-jumps (rest data))
        3 (recur one-jumps two-jumps (inc three-jumps) (rest data)))
      {:one-jumps one-jumps
       :two-jumps two-jumps
       :three-jumps three-jumps})))

(defn count-arrangements [max data]
  (let [data-as-set (set (conj data 0))]
    (loop [i (dec max)
           table (transient {(+ 0 max) 1
                             (+ 1 max) 0
                             (+ 2 max) 0
                             (+ 3 max) 0})]
      (if (< i 0)
        (get table 0)
        (if (get data-as-set i)
          (recur (dec i) (assoc! table i (+ (get table (+ 1 i))
                                            (get table (+ 2 i))
                                            (get table (+ 3 i)))))
          (recur (dec i) (assoc! table i 0)))))))

(defn problem-1 []
  (let [data (read-input-file "./resources/day10.input")
        data-outlet-and-device (conj data 0 (get-device-joltage data))
        {:keys [one-jumps three-jumps]} (count-joltage-jumps data-outlet-and-device)]
    (* one-jumps three-jumps)))

(defn problem-2 []
  (let [data (read-input-file "./resources/day10.input")
        device-joltage (get-device-joltage data)]
    (count-arrangements device-joltage data)))

(comment
  (problem-1)
  (problem-2))

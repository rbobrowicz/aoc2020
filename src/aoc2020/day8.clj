(ns aoc2020.day8
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[instruction argument] (str/split line #" ")]
    [(keyword instruction) (Integer/parseInt argument)]))

(defn read-input-file [path]
  (->> (slurp path)
       str/split-lines
       (map parse-line)
       (into [])))

(defn interpret [code]
  (loop [ip 0
         acc 0
         visited #{}]
    (cond
      (contains? visited ip) [:cycle acc visited]
      (>= ip (count code)) [:eof acc visited]
      :else
      (let [[inst arg] (get code ip)]
        (case inst
          :acc (recur (inc ip) (+ acc arg) (conj visited ip))
          :jmp (recur (+ ip arg) acc (conj visited ip))
          :nop (recur (inc ip) acc (conj visited ip)))))))

(defn corrected-program-candidates [code instructions]
  (letfn [(f [ip]
            (let [[inst _] (get code ip)]
              (case inst
                :acc nil
                :jmp (assoc-in code [ip 0] :nop)
                :nop (assoc-in code [ip 0] :jmp))))]
    (filter some? (map f instructions))))

(defn fix-program [code]
  (let [[_ _ visited] (interpret code)
        candidates (corrected-program-candidates code visited)]
    (->> candidates
         (map interpret)
         (filter #(= :eof (first %)))
         first)))

(defn problem-1 []
  (let [prog (read-input-file "./resources/day8.input")
        [_ accum _] (interpret prog)]
    accum))

(defn problem-2 []
  (let [prog (read-input-file "./resources/day8.input")
        [_ accum _] (fix-program prog)]
    accum))

(comment
  (problem-1)
  (problem-2))

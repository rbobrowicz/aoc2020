(ns aoc2020.day11
  (:require [clojure.string :as str]))

(defn to-keyword [c]
  (case c
    \. :floor
    \L :empty
    \# :full))

(defn parse-room [data]
  (let [lines (str/split-lines data)]
    {:row-length (count (first lines))
     :num-rows (count lines)
     :seats (->> data
                 (filter #(not= % \newline))
                 (mapv to-keyword))}))

(defn read-input-file [path]
  (->> (slurp path)
       (parse-room)))

(defn get-seat [row col {:keys [seats row-length]}]
  (get seats (+ col (* row-length row))))

(defn get-neighbors [row col {:keys [row-length num-rows] :as room}]
  (for [i (range (dec row) (+ 2 row))
        j (range (dec col) (+ 2 col))
        :when (and (< -1 i num-rows)
                   (< -1 j row-length)
                   (not (and (= row i)
                             (= col j))))]
    (get-seat i j room)))

(defn get-neighbor-in-direction [row col {:keys [num-rows row-length] :as room} row-inc col-inc]
  (let [new-row (+ row-inc row)
        new-col (+ col-inc col)]
    (when (and (< -1 new-row num-rows)
               (< -1 new-col row-length))
      (when-let [seat (get-seat new-row new-col room)]
        (case seat
          :floor (recur new-row new-col room row-inc col-inc)
          :empty nil
          :full :full)))))

(defn get-distant-neighbors [row col room]
  (for [i (range -1 2)
        j (range -1 2)
        :when (not (and (= i 0)
                        (= j 0)))
        :let [neighbor (get-neighbor-in-direction row col room i j)]
        :when (some? neighbor)]
    neighbor))

(defn seat-state-change [seat neighbor-tolerance neighbors]
  (let [full-neighbors (count (filter #(= % :full) neighbors))]
    (cond
      (and (= seat :empty)
           (= 0 full-neighbors))
      :full

      (and (= seat :full)
           (<= neighbor-tolerance full-neighbors))
      :empty

      :else
      seat)))

(defn run [{:keys [row-length num-rows] :as room}]
  (let [dirty (atom 0)
        new-seats (atom (transient []))]
    ;; This should probably be a map
    (doseq [i (range num-rows)
            j (range row-length)]
      (let [neighbors (get-neighbors i j room)
            seat (get-seat i j room)
            new-seat (seat-state-change seat 4 neighbors)]
        (swap! new-seats conj! new-seat)
        (when (not= seat new-seat)
          (swap! dirty inc))))
    (println "Dirty: " @dirty)
    (if (< 0 @dirty)
      (recur (assoc room :seats (persistent! @new-seats)))
      room)))

(defn run-2 [{:keys [row-length num-rows] :as room}]
  (let [dirty (atom 0)
        new-seats (atom (transient []))]
    ;; This should probably be a map
    (doseq [i (range num-rows)
            j (range row-length)]
      (let [neighbors (get-distant-neighbors i j room)
            seat (get-seat i j room)
            new-seat (seat-state-change seat 5 neighbors)]
        (swap! new-seats conj! new-seat)
        (when (not= seat new-seat)
          (swap! dirty inc))))
    (println "Dirty: " @dirty)
    (if (< 0 @dirty)
      (recur (assoc room :seats (persistent! @new-seats)))
      room)))

(defn count-occupied [{:keys [seats]}]
  (count (filter #(= :full %) seats)))

(defn problem-1 []
  (let [data (read-input-file "./resources/day11.input")]
    (count-occupied (run data))))

(defn problem-2 []
  (let [data (read-input-file "./resources/day11.input")]
    (count-occupied (run-2 data))))

(comment
  (problem-1)
  (problem-2))

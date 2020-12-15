(ns aoc2020.day12
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  [(-> line
        first
        str/lower-case
        keyword)
    (-> line
        (subs 1)
        (Integer/parseInt))])

(defn read-input-file [path]
  (->> (slurp path)
       (str/split-lines)
       (map parse-instruction)))

(defn turn [{:keys [facing] :as boat} cmd arg]
  (let [right-seq (cycle [:north :east :south :west])
        left-seq (cycle [:north :west :south :east])
        new-dir (case cmd
                  :l (first
                      (drop (quot arg 90)
                            (drop-while #(not= % facing) left-seq)))
                  :r (first
                      (drop (quot arg 90)
                            (drop-while #(not= % facing) right-seq))))]
    (assoc boat :facing new-dir)))

(defn process-instruction [{:keys [facing] :as boat} [cmd arg]]
  (case cmd
    :n (update boat :y-pos #(+ % arg))
    :s (update boat :y-pos #(- % arg))
    :e (update boat :x-pos #(+ % arg))
    :w (update boat :x-pos #(- % arg))
    :l (turn boat cmd arg)
    :r (turn boat cmd arg)
    :f (case facing
         :north (update boat :y-pos #(+ % arg))
         :south (update boat :y-pos #(- % arg))
         :east (update boat :x-pos #(+ % arg))
         :west (update boat :x-pos #(- % arg)))))

(defn rotate [boat cmd arg]
  (letfn [(f [{:keys [waypoint-rel-x waypoint-rel-y] :as boat} _]
            (case cmd
              :l (assoc boat
                        :waypoint-rel-y waypoint-rel-x
                        :waypoint-rel-x (* -1 waypoint-rel-y))
              :r (assoc boat
                        :waypoint-rel-y (* -1 waypoint-rel-x)
                        :waypoint-rel-x waypoint-rel-y)))]
    (reduce f boat (range (quot arg 90)))))

(defn forward [{:keys [x-pos y-pos waypoint-rel-x waypoint-rel-y] :as boat} arg]
  (assoc boat
         :x-pos (+ x-pos (* arg waypoint-rel-x))
         :y-pos (+ y-pos (* arg waypoint-rel-y))))

(defn process-instruction-2 [boat [cmd arg]]
  (case cmd
    :n (update boat :waypoint-rel-y #(+ % arg))
    :s (update boat :waypoint-rel-y #(- % arg))
    :e (update boat :waypoint-rel-x #(+ % arg))
    :w (update boat :waypoint-rel-x #(- % arg))
    :l (rotate boat cmd arg)
    :r (rotate boat cmd arg)
    :f (forward boat arg)))

(defn run [boat instructions f]
  (reduce f boat instructions))

(defn manhattan-distance [{:keys [x-pos y-pos]}]
  (+ (Math/abs x-pos)
     (Math/abs y-pos)))

(def starting-boat {:x-pos 0
                    :y-pos 0
                    :facing :east
                    :waypoint-rel-x 10
                    :waypoint-rel-y 1})

(defn problem-1 []
  (let [data (read-input-file "./resources/day12.input")]
    (manhattan-distance (run starting-boat data process-instruction))))

(defn problem-2 []
  (let [data (read-input-file "./resources/day12.input")]
    (manhattan-distance (run starting-boat data process-instruction-2))))

(comment
  (problem-1)
  (problem-2))

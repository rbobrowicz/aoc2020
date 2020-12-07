(ns aoc2020.day4
  (:require [clojure.string :as str]))

(defn split-passports [data]
  (str/split data #"\n\n"))

(defn read-input-file [path]
  (->> (slurp path)
       split-passports
       (map #(str/replace % \newline \space))))

(defn parse-passport [line]
  (let [bits (str/split line #" ")]
    (into {}
          (for [bit bits
                :let [[tag value] (str/split bit #":")]]
            [(keyword tag) value]))))

(def required-passport-tags [:byr :iyr :eyr :hgt :hcl :ecl :pid])

(defn int-in-range? [x low high]
  (try (<= low (Integer/parseInt x) high)
       (catch Exception _ false)))

(defn valid-byr? [byr] (int-in-range? byr 1920 2002))
(defn valid-iyr? [iyr] (int-in-range? iyr 2010 2020))
(defn valid-eyr? [eyr] (int-in-range? eyr 2020 2030))

(defn valid-hgt? [hgt]
  (when-let [[_ num unit] (re-find #"^(\d+)(cm$|in$)" hgt)]
    (or (and (= unit "in")
             (int-in-range? num 59 76))
        (and (= unit "cm")
             (int-in-range? num 150 193)))))

(defn valid-hcl? [hcl]
  (re-find #"^#[a-f0-9]{6}$" hcl))

(defn valid-ecl? [ecl]
  (some #{ecl} ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))

(defn valid-pid? [pid]
  (re-find #"^[0-9]{9}$" pid))

(defn valid-passport? [passport]
  (every? identity (map #(contains? passport %) required-passport-tags)))

(defn valid-passport-2? [passport]
  (and (valid-passport? passport)
       (valid-byr? (-> passport :byr))
       (valid-iyr? (-> passport :iyr))
       (valid-eyr? (-> passport :eyr))
       (valid-hgt? (-> passport :hgt))
       (valid-hcl? (-> passport :hcl))
       (valid-ecl? (-> passport :ecl))
       (valid-pid? (-> passport :pid))))

(defn problem-1 []
  (let [data (read-input-file "./resources/day4.input")
        passports (map parse-passport data)]
    (count (filter valid-passport? passports))))

(defn problem-2 []
  (let [data (read-input-file "./resources/day4.input")
        passports (map parse-passport data)]
    (count (filter valid-passport-2? passports))))

(comment
  (problem-1)
  (problem-2))

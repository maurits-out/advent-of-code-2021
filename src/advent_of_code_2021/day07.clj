(ns advent_of_code_2021.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-input []
  (let [input (slurp (io/resource "input-07.txt"))]
    (->> (string/split input #",")
         (map #(Integer/parseInt %)))))

(defn median [positions]
  (let [sorted-positions (sort positions)
        num-positions (count positions)
        mid-point (quot num-positions 2)]
    (if (odd? num-positions)
      (nth sorted-positions mid-point)
      (quot (+ (nth sorted-positions mid-point) (nth sorted-positions (dec mid-point))) 2))))

(defn calculate-fuel-part-1 [positions]
  (let [median (median positions)]
    (->> positions
         (map #(Math/abs ^int (- median %)))
         (apply +))))

(defn calc-increasing-fuel-costs [average p]
  (let [diff (Math/abs ^int (- average p))]
    (quot (* diff (inc diff)) 2)))

(defn average [positions]
  (quot (apply + positions) (count positions)))

(defn calculate-fuel-part-2 [positions]
  (let [avg (average positions)]
    (->> positions
         (map #(calc-increasing-fuel-costs avg %))
         (apply +))))

(defn -main []
  (let [positions (parse-input)]
    (println "Part 1:" (calculate-fuel-part-1 positions))
    (println "Part 2:" (calculate-fuel-part-2 positions))))

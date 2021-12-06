(ns advent_of_code_2021.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sum-nil #((fnil + 0) %1 %2))

(defn parse-input []
  (let [input (slurp (io/resource "input-06.txt"))]
    (->> (string/split input #",")
         (map #(Integer/parseInt %))
         frequencies)))

(defn next-day [state]
  (reduce-kv (fn [m k v] (if (zero? k)
                           (-> m
                               (assoc 8 v)
                               (update 6 #(sum-nil % v)))
                           (update m (dec k) #(sum-nil % v))))
    {} state))

(defn count-fish [initial-state after-days]
  (let [days (iterate next-day initial-state)]
    (apply + (vals (nth days after-days)))))

(defn -main []
  (let [initial-state (parse-input)]
    (println "Part 1:" (count-fish initial-state 80))
    (println "Part 2:" (count-fish initial-state 256))))

(ns advent_of_code_2021.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn count-increases [w]
  (->> (io/resource "input-01.txt")
    slurp
    string/split-lines
    (map #(Integer/parseInt %))
    (partition w 1)
    (map #(apply + %))
    (partition 2 1)
    (filter #(apply < %))
    count))

(defn -main []
  (println "Part 1:" (count-increases 1))
  (println "Part 2:" (count-increases 3)))

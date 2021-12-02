(ns advent_of_code_2021.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def part1 {"forward" (fn [[h d a] x] [(+ h x) d a])
            "down"    (fn [[h d a] x] [h (+ d x) a])
            "up"      (fn [[h d a] x] [h (- d x) a])})

(def part2 {"forward" (fn [[h d a] x] [(+ h x) (+ d (* a x)) a])
            "down"    (fn [[h d a] x] [h d (+ a x)])
            "up"      (fn [[h d a] x] [h d (- a x)])})

(defn apply-cmd-fn [part]
  (fn [state cmd]
    (let [[s1 s2] (string/split cmd #" ")
          x (Integer/parseInt s2)]
      (apply (part s1) [state x]))))

(defn follow-course [part]
  (->> (io/resource "input-02.txt")
    slurp
    string/split-lines
    (reduce (apply-cmd-fn part) [0 0 0])
    drop-last
    (apply *)))

(defn -main []
  (println "Part 1:" (follow-course part1))
  (println "Part 2:" (follow-course part2)))

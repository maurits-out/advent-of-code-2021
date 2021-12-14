(ns advent_of_code_2021.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def fold-fn {"x" (fn [line [x y]]
                    (if (> x line)
                      [(- (* 2 line) x) y]
                      [x y])),
              "y" (fn [line [x y]]
                    (if (> y line)
                      [x (- (* 2 line) y)]
                      [x y]))})

(defn parse-dot-coordinates [section]
  (->> (string/split-lines section)
       (map #(string/split % #","))
       (map #(vector (Integer/parseInt (first %)) (Integer/parseInt (second %))))
       (into #{})))

(defn parse-fold-instructions [section]
  (->> (string/split-lines section)
       (map #(next (re-matches #"fold along ([xy])=(\d+)" %)))
       (mapv (fn [[axis line]] {:axis axis :line (Integer/parseInt line)}))))

(defn parse-input []
  (let [input (slurp (io/resource "input-13.txt"))
        sections (string/split input #"\n\n")]
    {:dots              (parse-dot-coordinates (first sections)),
     :fold-instructions (parse-fold-instructions (second sections))}))

(defn fold [dots {:keys [axis line]}]
  (let [fn (partial (fold-fn axis) line)]
    (into #{} (map fn dots))))

(defn smallest-rectangle [dots]
  {:max-x (apply max (for [d dots] (first d)))
   :max-y (apply max (for [d dots] (second d)))})

(defn row2str [dots y max-x]
  (apply str (for [x (range (inc max-x))]
               (if (contains? (into #{} dots) [x y]) "#" " "))))

(defn plot-rows [dots]
  (let [{:keys [max-x max-y]} (smallest-rectangle dots)]
    (for [y (range (inc max-y))]
      (row2str dots y max-x))))

(defn plot [dots]
  (string/join "\n" (plot-rows dots)))

(defn part1 [{:keys [dots fold-instructions]}]
  (->> (first fold-instructions)
       (fold dots)
       count))

(defn part2 [{:keys [dots fold-instructions]}]
  (->> (reduce #(fold %1 %2) dots fold-instructions)
       plot))

(defn -main []
  (let [dots-and-instructions (parse-input)]
    (println "Part 1:" (part1 dots-and-instructions))
    (println "Part 2:")
    (println (part2 dots-and-instructions))))

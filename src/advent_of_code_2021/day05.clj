(ns advent_of_code_2021.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn sign [n]
  (if (pos-int? n) 1 -1))

(defn parse-line [l]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" l)]
    (vector (parse-int x1) (parse-int y1) (parse-int x2) (parse-int y2))))

(defn generate-points-vertical [x y1 y2]
  (let [diff (- y2 y1)
        dy (sign diff)]
    (->> (iterate #(+ % dy) y1)
         (map #(vector x %))
         (take (inc (* dy diff))))))

(defn generate-points-horizontal [x1 x2 y]
  (let [diff (- x2 x1)
        dx (sign diff)]
    (->> (iterate #(+ % dx) x1)
         (map #(vector % y))
         (take (inc (* dx diff))))))

(defn generate-points-diagonal [x1 y1 x2 y2]
  (let [diff-x (- x2 x1)
        diff-y (- y2 y1)
        dx (sign diff-x)
        dy (sign diff-y)]
    (->> (iterate #(inc %) 0)
         (map #(vector (+ x1 (* % dx)) (+ y1 (* % dy))))
         (take (inc (* dx diff-x))))))

(defn generate-points [vents diagonal-vents?]
  (apply concat (map (fn [[x1 y1 x2 y2]]
                       (cond
                         (= y1 y2) (generate-points-horizontal x1 x2 y1)
                         (= x1 x2) (generate-points-vertical x1 y1 y2)
                         diagonal-vents? (generate-points-diagonal x1 y1 x2 y2)
                         :else (seq [])))
                  vents)))

(defn count-overlapping [vents diagonal-vents?]
  (->> (generate-points vents diagonal-vents?)
       frequencies
       (filter #(>= (second %) 2))
       count))

(defn read-input []
  (->> (io/resource "input-05.txt")
       slurp
       string/split-lines
       (map parse-line)))

(defn -main []
  (let [vents (read-input)]
    (println "Part 1:" (count-overlapping vents false))
    (println "Part 2:" (count-overlapping vents true))))

(ns advent_of_code_2021.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def open-char? #{\( \[ \{ \<})
(def close-char {\( \), \[ \], \{ \}, \< \>})
(def score {\) 3, \] 57, \} 1197, \> 25137})
(def point-value {\) 1, \] 2, \} 3, \> 4})

(defn parse-input []
  (->> (io/resource "input-10.txt")
       slurp
       string/split-lines))

(defn calc-completion-score [stack]
  (reduce #(+ (* 5 %1) (point-value %2)) 0 stack))

(defn calc-scores [l]
  (loop [[c & cs] l
         stack '()]
    (cond
      (nil? c) {:syntax-error-score 0, :completion-score (calc-completion-score stack)}
      (open-char? c) (recur cs (conj stack (close-char c)))
      (= c (first stack)) (recur cs (next stack))
      :else {:syntax-error-score (score c)})))

(defn median [scores]
  (nth (sort scores) (quot (count scores) 2)))

(defn part1 [scores]
  (reduce #(+ %1 (:syntax-error-score %2)) 0 scores))

(defn part2 [scores]
  (->> (filter #(zero? (:syntax-error-score %)) scores)
       (map :completion-score)
       median))

(defn -main []
  (let [scores (map calc-scores (parse-input))]
    (println "Part 1:" (part1 scores))
    (println "Part 2:" (part2 scores))))

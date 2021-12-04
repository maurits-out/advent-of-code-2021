(ns advent_of_code_2021.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn binary-char-to-int [ch]
  (- (int ch) (int \0)))

(defn convert-line [l]
  (->> l
       (map-indexed (fn [pos ch] [pos (binary-char-to-int ch)]))))

(defn max-occurrence [freq]
  (key (apply max-key val freq)))

(defn min-occurrence [freq]
  (key (apply min-key val freq)))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn calculate-epsilon-rate [pos-freqs]
  (apply + (map (fn [[pos freq]] (* (exp 2 pos) (min-occurrence freq))) pos-freqs)))

(defn calculate-gamma-rate [pos-freqs]
  (apply + (map (fn [[pos freq]] (* (exp 2 pos) (max-occurrence freq))) pos-freqs)))

(defn read-input []
  (->> (io/resource "input-03.txt")
       slurp
       string/split-lines))

(defn count-and-group-by-position [ls]
  (->> ls
       (map convert-line)
       (apply concat)
       (group-by first)
       (reduce-kv #(assoc %1 %2 (frequencies (map second %3))) {})))

(defn part1 [pos-freqs]
  (* (calculate-gamma-rate pos-freqs) (calculate-epsilon-rate pos-freqs)))

(defn find-most-common-value [freqs]
  (let [zeros (freqs 0)
        ones (freqs 1)]
    (if (>= ones zeros) 1 0)))

(defn find-least-common-value [freqs]
  (let [zeros (freqs 0)
        ones (freqs 1)]
    (if (<= zeros ones) 0 1)))

(defn filter-numbers [numbers bit pos]
  (for [n numbers :when (= (binary-char-to-int (nth n pos)) bit)]
    n))

(defn find-oxygen-generator-rating [ls]
  (loop [numbers ls
         pos 0]
    (if (= (count numbers) 1)
      (Integer/parseInt (first numbers) 2)
      (let [pos-freqs (count-and-group-by-position numbers)
            most-common-value (find-most-common-value (pos-freqs pos))]
        (recur (filter-numbers numbers most-common-value pos) (inc pos))))))

(defn find-scrubber-generator-rating [ls]
  (loop [numbers ls
         pos 0]
    (if (= (count numbers) 1)
      (Integer/parseInt (first numbers) 2)
      (let [pos-freqs (count-and-group-by-position numbers)
            least-common-value (find-least-common-value (pos-freqs pos))]
        (recur (filter-numbers numbers least-common-value pos) (inc pos))))))

(defn part2 [ls]
  (* (find-oxygen-generator-rating ls) (find-scrubber-generator-rating ls)))

(defn -main []
  (let [ls (read-input)]
    (println "Part 1:" (part1 (count-and-group-by-position (map reverse ls))))
    (println "Part 2:" (part2 ls))))

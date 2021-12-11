(ns advent_of_code_2021.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as w]))

(defn char->number [ch]
  (- (int ch) (int \0)))

(defn create-grid [input]
  (into (hash-map)
    (apply concat (map-indexed
                    (fn [r l] (map-indexed (fn [c ch] [[r c] (char->number ch)]) l))
                    (string/split-lines input)))))

(defn read-input []
  (slurp (io/resource "input-11.txt")))

(defn inc-energy-level [state]
  (update state :grid #(w/walk (fn [[position level]] [position (inc level)]) identity %)))

(defn find-position-to-flash [grid flashed]
  (->> grid
       (filter (fn [[_ level]] (> level 9)))
       (remove (fn [[position _]] (flashed position)))
       ffirst))

(defn adjacent-positions [grid [row column]]
  (let [r (range -1 2)
        adjacent-positions (for [dr r, dc r,
                                 :when (not= dr dc 0)]
                             [(+ row dr) (+ column dc)])]
    (filter #(contains? (set (keys grid)) %) adjacent-positions)))

(defn flash [grid position]
  (reduce #(update %1 %2 inc) grid (adjacent-positions grid position)))

(defn reset-flashed-positions [grid flashed]
  (reduce #(assoc %1 %2 0) grid flashed))

(defn do-flashes [state]
  (loop [grid (:grid state) flashed #{}]
    (let [position (find-position-to-flash grid flashed)]
      (if (nil? position)
        {:grid (reset-flashed-positions grid flashed), :flash-count (count flashed)}
        (recur (flash grid position) (conj flashed position))))))

(defn initial-state [input]
  {:grid (create-grid input) :flash-count 0})

(defn states [input]
  (iterate (comp do-flashes inc-energy-level) (initial-state input)))

(defn not-all-flashed? [state]
  (not= (:flash-count state) (count (keys (:grid state)))))

(defn part1 [states]
  (->> (take 101 states)
       (map :flash-count)
       (reduce +)))

(defn part2 [states]
  (->> (take-while not-all-flashed? states)
       count))

(defn -main []
  (let [s (states (read-input))]
    (println "Part 1:" (part1 s))
    (println "Part 2:" (part2 s))))

(ns advent_of_code_2021.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn char->number [ch]
  (- (int ch) (int \0)))

(defn create-grid [input]
  (into (hash-map)
    (apply concat (map-indexed
                    (fn [r l]
                      (map-indexed (fn [c ch] [[r c] (char->number ch)]) l))
                    (string/split-lines input)))))

(defn read-input []
  (slurp (io/resource "input-11.txt")))

(defn inc-energy-level [state]
  (update state :grid (fn [grid] (reduce-kv #(assoc %1 %2 (inc %3)) {} grid))))

(defn find-position-to-flash [grid flashed]
  (->> grid
       (filter (fn [[_ level]] (> level 9)))
       (remove (fn [[position _]] (flashed position)))
       ffirst))

(defn adjacent-positions [grid [row column :as position]]
  (let [r (range -1 2),
        all-pos (set (keys grid))]
    (for [dr r, dc r,
          :let [p [(+ row dr) (+ column dc)]]
          :when (and (not= p position) (all-pos p))]
      p)))

(defn flash [grid position]
  (reduce #(update %1 %2 inc) grid (adjacent-positions grid position)))

(defn reset-flashed-positions [grid flashed]
  (reduce #(assoc %1 %2 0) grid flashed))

(defn do-flashes [state]
  (loop [grid (:grid state) flashed #{}]
    (if-let [position (find-position-to-flash grid flashed)]
      (recur (flash grid position) (conj flashed position))
      {:grid (reset-flashed-positions grid flashed), :flash-count (count flashed)})))

(defn initial-state [input]
  {:grid (create-grid input), :flash-count 0})

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

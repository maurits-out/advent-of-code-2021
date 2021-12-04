(ns advent_of_code_2021.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-board-row [row-num row-str]
  (let [numbers (string/split (string/trim row-str) #"\s+")]
    (map-indexed #(vector [row-num %1] (Integer/parseInt %2)) numbers)))

(defn extract-rows-and-columns [board]
  (let [partitions (concat
                     (vals (group-by first (keys board)))
                     (vals (group-by second (keys board))))]
    (for [p partitions]
      (set (map #(board %) p)))))

(defn parse-board [section]
  (let [pos-number (into (hash-map)
                     (apply concat (map-indexed #(parse-board-row %1 %2) (string/split-lines section))))]
    {:rows-columns (extract-rows-and-columns pos-number),
     :numbers (into (hash-set) (vals pos-number))}))

(defn parse-boards [sections]
  (into (hash-map) (map-indexed (fn [idx s] [idx (parse-board s)]) sections)))

(defn parse-numbers-row [line]
  (map #(Integer/parseInt %) (string/split line #",")))

(defn parse-input []
  (let [input (slurp (io/resource "input-04.txt"))
        sections (string/split input #"\n\n")]
    [(parse-numbers-row (first sections)) (parse-boards (rest sections))]))

(defn winner? [board played]
  (some #(set/subset? % played) (:rows-columns board)))

(defn filter-winning-boards [id->board played]
  (->> id->board
       (filter (fn [[_ board]] (winner? board played)))
       (map first)
       set
       not-empty))

(defn sum-of-unmarked-numbers [board played]
  (apply + (set/difference (:numbers board) played)))

(defn calculate-score [board played-numbers last-played]
  (* (sum-of-unmarked-numbers board played-numbers) last-played))

(defn remove-winning-boards [remaining-board-ids winning-board-ids]
  (remove #(contains? winning-board-ids %) remaining-board-ids))

(defn play-part1 [numbers id->board]
  (loop [[n & ns] numbers
         played (hash-set)]
    (let [updated-played (conj played n)]
      (if-let [winning-board-id (first (filter-winning-boards id->board updated-played))]
        (calculate-score (id->board winning-board-id) updated-played n)
        (recur ns updated-played)))))

(defn play-part2 [numbers id->board]
  (loop [[n & ns] numbers
         played (hash-set)
         remaining-ids (keys id->board)]
    (let [updated-played (conj played n)]
      (if-let [winning-board-ids (filter-winning-boards (select-keys id->board remaining-ids) updated-played)]
        (if (= 1 (count remaining-ids))
          (calculate-score (id->board (first winning-board-ids)) updated-played n)
          (recur ns updated-played (remove-winning-boards remaining-ids winning-board-ids)))
        (recur ns updated-played remaining-ids)))))

(defn -main []
  (let [[numbers id->board] (parse-input)]
    (println "Part 1:" (play-part1 numbers id->board))
    (println "Part 2:" (play-part2 numbers id->board))))

(ns advent_of_code_2021.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [data.deque :refer [deque add-last remove-first peek-first]]))

(defn parse-line [l]
  (mapv #(- (int %) (int \0)) l))

(defn parse-input []
  (->> (io/resource "input-09.txt")
       slurp
       string/split-lines
       (mapv parse-line)
       to-array-2d))

(defn generate-coordinates [heightmap]
  (for [r (range (alength heightmap))
        c (range (alength (aget heightmap r)))]
    [r c]))

(defn is-valid-location? [heightmap [row column]]
  (and (<= 0 row) (< row (alength heightmap)) (<= 0 column) (< column (alength (aget heightmap row)))))

(defn get-adjacent-locations [heightmap location]
  (->> (map #(mapv + location %) [[-1 0] [0 1] [1 0] [0 -1]])
       (filter #(is-valid-location? heightmap %))))

(defn low-point? [heightmap [row column :as location]]
  (let [adjacent (get-adjacent-locations heightmap location)
        height (aget heightmap row column)]
    (every? (fn [[r c]] (< height (aget heightmap r c))) adjacent)))

(defn find-low-points [heightmap]
  (->> (generate-coordinates heightmap)
       (filter (partial low-point? heightmap))))

(defn part1 [heightmap low-points]
  (->> low-points
       (map (fn [[r c]] (inc (aget heightmap r c))))
       (apply +)))

(defn next-locations-to-visit [heightmap visited location]
  (->> (get-adjacent-locations heightmap location)
       (remove #(visited %))
       (remove #(= 9 (aget heightmap (first %) (second %))))))

(defn basin-size [heightmap low-point]
  (loop [queue (deque low-point)
         visited #{low-point}
         current-size 1]
    (if (empty? queue)
      current-size
      (let [next-locations (next-locations-to-visit heightmap visited (peek-first queue))]
        (recur (into (remove-first queue) next-locations)
          (into visited next-locations)
          (+ current-size (count next-locations)))))))

(defn part2 [heightmap low-points]
  (->> low-points
       (map (partial basin-size heightmap))
       (sort (comp - compare))
       (take 3)
       (apply *)))

(defn -main []
  (let [heightmap (parse-input)
        low-points (find-low-points heightmap)]
    (println "Part 1:" (part1 heightmap low-points))
    (println "Part 2:" (part2 heightmap low-points))))

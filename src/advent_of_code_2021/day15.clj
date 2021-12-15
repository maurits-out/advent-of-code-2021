(ns advent_of_code_2021.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as s]))

(def start-pos [0 0])

(defn char->number [ch]
  (- (int ch) (int \0)))

(defn create-cave [input]
  (into (hash-map)
        (apply concat (map-indexed
                        (fn [r l]
                          (map-indexed (fn [c ch] [[r c] (char->number ch)]) l))
                        (string/split-lines input)))))

(defn read-input []
  (slurp (io/resource "input-15.txt")))

(defn get-neighbors [[row column] visited size]
  (->> [[row (inc column)] [(inc row) column] [row (dec column)] [(dec row) column]]
       (filter (fn [[r c]] (and (>= r 0) (< r size) (>= c 0) (< c size))))
       (remove (fn [n] (contains? visited n)))
       (into #{})))

(defn find-position-with-lowest-distance [nodes distances]
  (apply min-key (fn [n] (get distances n Long/MAX_VALUE)) nodes))

(defn dijkstra [cave size]
  (loop [visited #{start-pos}
         current-positions (get-neighbors start-pos visited size)
         distances (reduce #(assoc %1 %2 (cave %2)) {start-pos 0} current-positions)]
    (if (empty? current-positions)
      (distances [(dec size) (dec size)])
      (let [position (find-position-with-lowest-distance current-positions distances)
            updated-visited (conj visited position)
            unvisited-neighbors (get-neighbors position updated-visited size)
            updated-distances (reduce (fn [d n] (assoc d n (min (+ (d position) (cave n)) (get d n Long/MAX_VALUE))))
                                      distances
                                      unvisited-neighbors)
            updated-positions (s/union (disj current-positions position) unvisited-neighbors)]
        (recur updated-visited updated-positions updated-distances)))))

(defn extended-case-fn [cave dim]
  (fn [[row column]]
    (let [original-cave-value (cave [(rem row dim) (rem column dim)])
          updated-with-tile (+ original-cave-value (quot row dim) (quot column dim))
          wrapped-back (inc (rem (dec updated-with-tile) 9))]
      wrapped-back)))

(defn -main []
  (let [cave (create-cave (read-input))
        dimension 100
        extended-cave (extended-case-fn cave dimension)]
    (println "Part 1:" (dijkstra cave dimension))
    (println "Part 2:" (dijkstra extended-cave (* 5 dimension)))))

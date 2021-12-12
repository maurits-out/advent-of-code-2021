(ns advent_of_code_2021.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-input []
  (->> (io/resource "input-12.txt")
       slurp
       string/split-lines
       (map #(string/split % #"-"))
       (map #(vector % (reverse %)))
       (apply concat)
       (group-by first)
       (map (fn [[k v]] [k (vec (flatten (map second v)))]))
       (into (hash-map))))

(defn increment-visit-count [visit-count cave]
  (update visit-count cave #((fnil + 0) % 1)))

(defn push-caves [stack visit-count caves]
  (reduce #(conj %1 [%2 (increment-visit-count visit-count %2)]) stack caves))

(defn small-cave? [cave]
  (= cave (string/lower-case cave)))

(defn small-and-visited-twice? [[cave count]]
  (and (small-cave? cave) (= count 2)))

(defn no-small-caves-visited-twice? [visit-count]
  (not-any? #(small-and-visited-twice? %) visit-count))

(defn visit-allowed? [cave visit-count allow-2-visits?]
  (cond
    (= cave "start") false
    (small-cave? cave) (case (visit-count cave)
                         nil true
                         1 (and allow-2-visits? (no-small-caves-visited-twice? visit-count))
                         false)
    :else true))

(defn filter-neighbors [visit-count allow-2-visits? neighbors]
  (filter #(visit-allowed? % visit-count allow-2-visits?) neighbors))

(defn count-paths [adjacency-list allow-2-visits?]
  (loop [stack '(["start" {}])
         count 0]
    (if-let [[[cave visit-count] & cs] stack]
      (if (= cave "end")
        (recur cs (inc count))
        (let [neighbor-caves (->> (get adjacency-list cave [])
                                  (filter-neighbors visit-count allow-2-visits?))]
          (recur (push-caves cs visit-count neighbor-caves) count)))
      count)))

(defn -main []
  (let [adjacency-list (parse-input)]
    (println "Part 1:" (count-paths adjacency-list false))
    (println "Part 2:" (count-paths adjacency-list true))))

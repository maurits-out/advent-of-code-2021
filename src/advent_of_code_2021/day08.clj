(ns advent_of_code_2021.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as s]))

(def segments->digit {#{:a :b :c :e :f :g}    0,
                      #{:c :f}                1,
                      #{:a :c :d :e :g}       2,
                      #{:a :c :d :f :g}       3,
                      #{:b :c :d :f}          4,
                      #{:a :b :d :f :g}       5,
                      #{:a :b :d :e :f :g}    6,
                      #{:a :c :f}             7,
                      #{:a :b :c :d :e :f :g} 8,
                      #{:a :b :c :d :f :g}    9})

(def valid-segments (set (keys segments->digit)))

(defn parse-line [line]
  (let [[left right] (string/split line #" \| ")]
    {:signal-patterns (string/split left #" "), :output-patterns (string/split right #" ")}))

(defn parse-input []
  (->> (io/resource "input-08.txt")
       slurp
       string/split-lines
       (mapv parse-line)))

(defn part1 [entries]
  (->> entries
       (map :output-patterns)
       (apply concat)
       (filter #(contains? #{2 4 3 7} (count %)))
       count))

(defn map-wires-to-segment-for-pattern [pattern]
  (let [candidate-segments (->> valid-segments
                                (filter #(= (count pattern) (count %)))
                                (apply s/union))]
    (into (hash-map)
      (for [wire pattern] [wire candidate-segments]))))

(defn map-wires-to-possible-segments [signal-patterns]
  (->> signal-patterns
       (map map-wires-to-segment-for-pattern)
       (apply merge-with #(s/intersection %1 %2))))

(defn pattern-maps-to-valid-digit? [pattern wire->segment]
  (let [segments (set (map wire->segment pattern))]
    (contains? valid-segments segments)))

(defn is-valid-candidate-mapping [signal-patterns wire->segment]
  (every? #(pattern-maps-to-valid-digit? % wire->segment) signal-patterns))

(defn search-candidate-mappings-fn [wire->possible-segments]
  (fn search-candidate-mappings [wires-to-assign mapping-under-construction]
    (if (empty? wires-to-assign)
      [mapping-under-construction]
      (let [[w & ws] wires-to-assign
            possible-segments (wire->possible-segments w)
            segments-already-mapped (set (vals mapping-under-construction))
            remaining-possible-segments (s/difference possible-segments segments-already-mapped)]
        (apply concat (for [s remaining-possible-segments
                            :let [updated-mapping (assoc mapping-under-construction w s)]]
                        (search-candidate-mappings ws updated-mapping)))))))

(defn map-wires-to-segments [signal-patterns]
  (let [wire->possible-segments (map-wires-to-possible-segments signal-patterns)
        search-candidate-mappings (search-candidate-mappings-fn wire->possible-segments)
        candidate-mappings (search-candidate-mappings (keys wire->possible-segments) {})]
    (->> candidate-mappings
         (filter (partial is-valid-candidate-mapping signal-patterns))
         first)))

(defn map-pattern-to-digit [mapping output-pattern]
  (let [segment (set (map mapping output-pattern))]
    (segments->digit segment)))

(defn calculate-output-value [output-patterns mapping]
  (reduce #(+ (* %1 10) (map-pattern-to-digit mapping %2)) 0 output-patterns))

(defn calculate-output-of-entry [{:keys [signal-patterns output-patterns]}]
  (->> (map-wires-to-segments signal-patterns)
       (calculate-output-value output-patterns)))

(defn part2 [entries]
  (reduce #(+ %1 (calculate-output-of-entry %2)) 0 entries))

(defn -main []
  (let [entries (parse-input)]
    (println "Part 1:" (part1 entries))
    (println "Part 2:" (part2 entries))))

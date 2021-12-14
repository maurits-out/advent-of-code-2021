(ns advent_of_code_2021.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-rules [lines]
  (into {} (map #(vector (subs % 0 2) (subs % 6 7)) lines)))

(defn parse-input []
  (let [lines (->> (io/resource "input-14.txt")
                   slurp
                   string/split-lines)]
    {:template (first lines) :rules (parse-rules (drop 2 lines))}))

(defn group-by-and-sum [coll]
  (->> (group-by first coll)
       (map (fn [[key values]] [key (reduce + (map second values))]))
       (into {})))

(defn initial-pair-counts [template]
  (->> (partition 2 1 template)
       (map #(vector (string/join %) 1))
       group-by-and-sum))

(defn apply-rule [left-hand right-hand count]
  (vector [(str (subs left-hand 0 1) right-hand) count]
          [(str right-hand (subs left-hand 1 2)) count]))

(defn execute-step [rules pair->count]
  (->> (map (fn [[pair count]] (apply-rule pair (rules pair) count)) pair->count)
       (apply concat)
       group-by-and-sum))

(defn steps [template rules]
  (iterate #(execute-step rules %) (initial-pair-counts template)))

(defn count-by-char [pair->count first-char-in-template]
  (let [char-counts (->> (map (fn [[pair count]] [(subs pair 1 2) count]) pair->count)
                         group-by-and-sum)]
    (update char-counts first-char-in-template #((fnil + 0) % 1))))

(defn calc-difference [char->count]
  (- (val (apply max-key val char->count)) (val (apply min-key val char->count))))

(defn apply-pair-insertion [template rules step-cnt]
  (let [step-sequence (steps template rules)
        pair->count (nth step-sequence step-cnt)
        char->count (count-by-char pair->count (subs template 0 1))]
    (calc-difference char->count)))

(defn -main []
  (let [{:keys [template rules]} (parse-input)]
    (println "Part 1:" (apply-pair-insertion template rules 10))
    (println "Part 2:" (apply-pair-insertion template rules 40))))

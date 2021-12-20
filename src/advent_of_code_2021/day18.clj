(ns advent_of_code_2021.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn is-pair-of-regular-numbers?
  "Check if the substring of s starting at position idx is a pair of regular numbers [x,y] and returns a pair containing the index and the pair itself."
  [s idx]
  (let [m (re-pos #"(\[\d+,\d+\])" (subs s idx))]
    (if (contains? m 0)
      [idx (get m 0)]
      nil)))

(defn find-pair-of-regular-numbers-nested-inside-four-pairs [s]
  (loop [idx 0
         depth 0]
    (cond
      (= idx (count s)) nil
      (= (nth s idx) \[) (if (and (= depth 4) (is-pair-of-regular-numbers? s idx))
                           (is-pair-of-regular-numbers? s idx)
                           (recur (inc idx) (inc depth)))
      (= (nth s idx) \]) (recur (inc idx) (dec depth))
      :else (recur (inc idx) depth))))

(defn extract-numbers-from-pair [s idx]
  (let [[_ first second] (re-matches #"\[(\d+),(\d+)\].*" (subs s idx))]
    [(Integer/parseInt first) (Integer/parseInt second)]))

(defn find-number-left-from
  "Returns a vector consisting of the index and the value of the right-most number in the substring of s starting at 0 up to (not-including) pair-idx"
  [s pair-idx]
  (let [m (re-pos #"(\d+)" (subs s 0 pair-idx))]
    (if (empty? m)
      nil
      (let [[idx number] (apply max-key key m)]
        [idx number]))))

(defn find-number-right-from
  "Returns a vector consisting of the index and the value of the left-most number in the substring of s starting at pair-idx"
  [s pair-idx]
  (let [m (re-pos #"(\d+)" (subs s pair-idx))]
    (if (empty? m)
      nil
      (let [[idx number] (apply min-key key m)]
        [(+ pair-idx idx) number]))))

(defn explode-pair [snailfish [pos pair-to-explode]]
  (let [[pos-number-left number-left] (find-number-left-from snailfish pos)
        [pos-number-right number-right] (find-number-right-from snailfish (+ pos (count pair-to-explode)))
        [exploding-number-left exploding-number-right] (extract-numbers-from-pair snailfish pos)
        explode-to-right (if pos-number-right
                           (str (subs snailfish 0 pos-number-right) (+ (Integer/parseInt number-right) exploding-number-right) (subs snailfish (+ pos-number-right (count number-right))))
                           snailfish)
        replace-exploding-pair (str (subs snailfish 0 pos) "0" (subs explode-to-right (+ pos (count pair-to-explode))))]
    (if pos-number-left
      (str (subs snailfish 0 pos-number-left) (+ (Integer/parseInt number-left) exploding-number-left) (subs replace-exploding-pair (+ pos-number-left (count number-left))))
      replace-exploding-pair)))

(defn split [snailfish]
  (let [m (re-pos #"(\d{2})" snailfish)]
    (if (empty? m)
      snailfish
      (let [[idx number-str] (apply min-key key m)
            number (Integer/parseInt number-str)]
        (str (subs snailfish 0 idx) "[" (quot number 2) "," (- number (quot number 2)) "]" (subs snailfish (+ idx 2)))))))

(defn reduce-action [snailfish]
  (let [pair-to-explode (find-pair-of-regular-numbers-nested-inside-four-pairs snailfish)]
    (if (not (nil? pair-to-explode))
      (explode-pair snailfish pair-to-explode)
      (split snailfish))))

(defn reduce-snailfish [snailfish]
  (loop [current snailfish]
    (let [updated (reduce-action current)]
      (if (= current updated)
        current
        (recur updated)))))

(defn add-snailfishes [snailfish1 snailfish2]
  (str "[" snailfish1 "," snailfish2 "]"))

(defn add-snailfish-list [lines]
  (reduce (fn [sf1 sf2] (reduce-snailfish (add-snailfishes sf1 sf2))) (first lines) (rest lines)))

(defn add-snailfish-list-from-file [file]
  (->> (io/resource file)
       slurp
       string/split-lines
       add-snailfish-list))

(declare magnitude-of-pair)

(defn parse-magnitude-subexpression [snailfish pos]
  (let [current (nth snailfish pos)]
    (if (Character/isDigit ^char current)
      [(- (int current) (int \0)) (inc pos)]
      (magnitude-of-pair snailfish pos))))

(defn magnitude-of-pair [snailfish pos]
  (let [[left i] (parse-magnitude-subexpression snailfish (inc pos))
        [right j] (parse-magnitude-subexpression snailfish (inc i))]
    [(+ (* 3 left) (* 2 right)) (inc j)]))

(defn part1 []
  (let [final-sum (add-snailfish-list-from-file "input-18.txt")]
    (first (magnitude-of-pair final-sum 0))))

(defn part2 []
  (let [snailfishes (->> (io/resource "input-18.txt")
                         slurp
                         string/split-lines)]
    (apply max (for [sf1 snailfishes
                     sf2 snailfishes
                     :when (not= sf1 sf2)]
                 (first (magnitude-of-pair (reduce-snailfish (add-snailfishes sf1 sf2)) 0))))))
(defn -main []
  (println "Part 1:" (part1))
  (println "Part 2:" (part2)))
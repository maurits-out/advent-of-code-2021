(ns advent_of_code_2021.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn re-pos
  "Utility method for regular expression re matches and their position in s."
  [re s]
  (loop [m (re-matcher re s), res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn pair-of-regular-numbers?
  "Check if the substring of s starting at position pos is a pair of regular numbers [x,y] and returns a vector containing the position and the pair itself."
  [s pos]
  (let [m (re-pos #"\[\d+,\d+\]" (subs s pos))]
    (if (contains? m 0)
      [pos (get m 0)]
      nil)))

(defn first-pair-of-regular-numbers-nested-inside-4-pairs [s]
  "Looks up the first pair of regular numbers in s at level 4. Returns a vector containing the position and the pair."
  (loop [pos 0, depth 0]
    (cond
      (= pos (count s)) nil
      (and (= (nth s pos) \[) (= depth 4)) (or (pair-of-regular-numbers? s pos) (recur (inc pos) (inc depth)))
      (= (nth s pos) \[) (recur (inc pos) (inc depth))
      (= (nth s pos) \]) (recur (inc pos) (dec depth))
      :else (recur (inc pos) depth))))

(defn extract-numbers-from-pair
  "Returns a vector containing the numbers in a pair [x,y] at position pos in s."
  [s pos]
  (let [[_ first second] (re-matches #"\[(\d+),(\d+)\].*" (subs s pos))]
    [(Integer/parseInt first) (Integer/parseInt second)]))

(defn find-number-left-from
  "Returns a vector containing the position and the value of the right-most number in the substring of s starting at 0 up to (not-including) end"
  [s end]
  (let [m (re-pos #"\d+" (subs s 0 end))]
    (if (empty? m)
      nil
      (let [[pos number] (apply max-key key m)]
        [pos number]))))

(defn find-number-right-from
  "Returns a vector consisting of the index and the value of the left-most number in the substring of s starting at start"
  [s start]
  (let [m (re-pos #"\d+" (subs s start))]
    (if (empty? m)
      nil
      (let [[idx number] (apply min-key key m)]
        [(+ start idx) number]))))

(defn explode-pair
  "Explodes the pair pair-to-explode in snail-fish at position pos"
  [snail-fish [pos pair-to-explode]]
  (let [[pos-number-left number-left] (find-number-left-from snail-fish pos)
        [pos-number-right number-right] (find-number-right-from snail-fish (+ pos (count pair-to-explode)))
        [exploding-number-left exploding-number-right] (extract-numbers-from-pair snail-fish pos)
        explode-to-right (if pos-number-right
                           (str (subs snail-fish 0 pos-number-right)
                                (+ (Integer/parseInt number-right) exploding-number-right)
                                (subs snail-fish (+ pos-number-right (count number-right))))
                           snail-fish)
        replace-exploding-pair (str (subs snail-fish 0 pos)
                                    "0"
                                    (subs explode-to-right (+ pos (count pair-to-explode))))
        explode-to-left (if pos-number-left
                          (str (subs snail-fish 0 pos-number-left)
                               (+ (Integer/parseInt number-left) exploding-number-left)
                               (subs replace-exploding-pair (+ pos-number-left (count number-left))))
                          replace-exploding-pair)]
    explode-to-left))

(defn number->pair [s]
  (let [number (Integer/parseInt s)]
    (str "[" (quot number 2) "," (- number (quot number 2)) "]")))

(defn split
  "Performs the split operation if possible"
  [snail-fish]
  (let [m (re-pos #"\d{2}" snail-fish)]
    (if (empty? m)
      snail-fish
      (let [[pos s] (apply min-key key m)]
        (str (subs snail-fish 0 pos) (number->pair s) (subs snail-fish (+ pos 2)))))))

(defn single-reduce [snail-fish]
  "Executes a single reduce"
  (if-let [pair-to-explode (first-pair-of-regular-numbers-nested-inside-4-pairs snail-fish)]
    (explode-pair snail-fish pair-to-explode)
    (split snail-fish)))

(defn reduce-snail-fish [snail-fish]
  (loop [current snail-fish]
    (let [updated (single-reduce current)]
      (if (= current updated)
        current
        (recur updated)))))

(defn add [snail-fish1 snail-fish2]
  (let [combined (str "[" snail-fish1 "," snail-fish2 "]")]
    (reduce-snail-fish combined)))

(defn sum-snail-fishes [file-name]
  (->> (io/resource file-name)
       slurp
       string/split-lines
       (reduce add)))

(declare magnitude-of-pair)

(defn magnitude-subexpression [snail-fish pos]
  (let [current (nth snail-fish pos)]
    (if (Character/isDigit ^char current)
      [(- (int current) (int \0)) (inc pos)]
      (magnitude-of-pair snail-fish pos))))

(defn magnitude-of-pair [snail-fish pos]
  (let [[left i] (magnitude-subexpression snail-fish (inc pos))
        [right j] (magnitude-subexpression snail-fish (inc i))]
    [(+ (* 3 left) (* 2 right)) (inc j)]))

(defn magnitude [snail-fish]
  (first (magnitude-of-pair snail-fish 0)))

(defn part1 []
  (magnitude (sum-snail-fishes "input-18.txt")))

(defn part2 []
  (let [snail-fishes (->> (io/resource "input-18.txt")
                          slurp
                          string/split-lines)]
    (apply max (for [sf1 snail-fishes, sf2 snail-fishes
                     :when (not= sf1 sf2)
                     :let [sum (add sf1 sf2)]]
                 (magnitude sum)))))

(defn -main []
  (println "Part 1:" (part1))
  (println "Part 2:" (part2)))
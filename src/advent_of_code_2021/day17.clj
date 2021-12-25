(ns advent_of_code_2021.day17)

(def min-x 96)
(def max-x 125)
(def min-y -144)
(def max-y -98)

(defn part1 []
  (loop [vel-y (- (inc min-y)), y 0]
    (if (zero? vel-y)
      y
      (recur (dec vel-y) (+ y vel-y)))))

(defn in-target-range? [start-vel-x start-vel-y]
  (loop [x 0, y 0, vel-x start-vel-x, vel-y start-vel-y]
    (cond
      (and (<= min-x x max-x) (<= min-y y max-y)) true
      (or (> x max-x) (< y min-y)) false
      :else (recur (+ x vel-x) (+ y vel-y) (max 0 (dec vel-x)) (dec vel-y)))))

(defn part2 []
  (count (for [vel-x (range 0 (inc max-x)), vel-y (range min-y (- min-y))
               :when (in-target-range? vel-x vel-y)]
           [vel-x vel-y])))

(defn -main []
  (println "Part 1:" (part1))
  (println "Part 2:" (part2)))
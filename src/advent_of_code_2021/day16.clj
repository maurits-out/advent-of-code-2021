(ns advent_of_code_2021.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def hex->bin {\0 "0000", \1 "0001", \2 "0010", \3 "0011", \4 "0100", \5 "0101", \6 "0110", \7 "0111",
               \8 "1000", \9 "1001", \A "1010", \B "1011", \C "1100", \D "1101", \E "1110" \F "1111"})

(defn read-input []
  (->> (io/resource "input-16.txt")
       slurp
       string/trim-newline
       (map hex->bin)
       (apply concat)))

(defn convert-binary [binary]
  (Long/parseLong (apply str binary) 2))

(declare parse-packet)

(defn parse-packet-header [bits]
  (let [version (convert-binary (take 3 bits))
        type (convert-binary (take 3 (drop 3 bits)))]
    {:version version, :type type}))

(defn parse-literal [bits]
  (loop [b bits
         binary-number []]
    (let [marker (first b)
          literal-data (take 4 (next b))]
      (if (= marker \0)
        {:number (convert-binary (concat binary-number literal-data)), :remaining-bits (drop 5 b)}
        (recur (drop 5 b) (concat binary-number literal-data))))))

(defn parse-packets [bits]
  (loop [b bits
         packets []]
    (if (< (count b) 6)
      packets
      (let [{:keys [packet remaining-bits]} (parse-packet b)]
        (recur remaining-bits (conj packets packet))))))

(defn parse-n-packets [n bits]
  (loop [b bits
         result []]
    (if (= (count result) n)
      {:sub-packets result, :remaining-bits b}
      (let [{:keys [packet remaining-bits]} (parse-packet b)]
        (recur remaining-bits (conj result packet))))))

(defn parse-operator [binary]
  (let [length-type-id (first binary)]
    (case length-type-id
      \0 (let [length (convert-binary (take 15 (next binary)))
               sub-packets (parse-packets (take length (drop 16 binary)))]
           {:sub-packets sub-packets, :remaining-bits (drop (+ 16 length) binary)})
      \1 (let [n (convert-binary (take 11 (next binary)))
               {:keys [sub-packets remaining-bits]} (parse-n-packets n (drop 12 binary))]
           {:sub-packets sub-packets, :remaining-bits remaining-bits}))))

(defn parse-packet [bits]
  (let [packet (parse-packet-header bits)]
    (case (:type packet)
      4 (let [{:keys [number remaining-bits]} (parse-literal (drop 6 bits))]
          {:packet (assoc packet :payload number) :remaining-bits remaining-bits})
      (let [{:keys [sub-packets remaining-bits]} (parse-operator (drop 6 bits))]
        {:packet (assoc packet :payload sub-packets) :remaining-bits remaining-bits}))))

(defn evaluate [{:keys [type payload]}]
  (case type
    0 (apply + (map evaluate payload))
    1 (apply * (map evaluate payload))
    2 (apply min (map evaluate payload))
    3 (apply max (map evaluate payload))
    4 payload
    5 (if (> (evaluate (first payload)) (evaluate (second payload))) 1 0)
    6 (if (< (evaluate (first payload)) (evaluate (second payload))) 1 0)
    7 (if (= (evaluate (first payload)) (evaluate (second payload))) 1 0)))

(defn sum-of-versions [{:keys [version type payload]}]
  (+ version (if (not= type 4) (apply + (map sum-of-versions payload)) 0)))

(defn -main []
  (let [root-packet (->> (read-input)
                         parse-packet
                         :packet)]
    (println "Part 1:" (sum-of-versions root-packet))
    (println "Part 2:" (evaluate root-packet))))

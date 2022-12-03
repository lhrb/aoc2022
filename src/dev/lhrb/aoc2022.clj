(ns dev.lhrb.aoc2022
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn foo
  "I don't do a whole lot."
  [x]
  (prn x "Hello, World!"))


(def in (slurp "resources/input"))

(->> in
     (str/split-lines)
     (map parse-long)
     (partition-by nil?)
     (map #(apply (fnil + 0) %))
     (sort >)
     (take 3)
     (apply +))

(parse-long "1891")

(def in2 (slurp "resources/input2"))
(def rounds
 (->> in2
      (str/split-lines)
      (map #(str/split % #" "))
      (take-last 100)))

(require '[clojure.core.match :refer [match]])


(defn convert [[a b]]
  [(get {"A" "R" "B" "P" "C" "S"} a)
   (get {"X" "R" "Y" "P" "Z" "S"} b)])

(->> rounds
     (map convert))

(defn points [[a b]]
  (+
   (get {"X" 1 "Y" 2 "Z" 3} b)
   (match [a b]
          ["A" "Y"] 6
          ["B" "Z"] 6
          ["C" "X"] 6
          ["A" "X"] 3
          ["B" "Y"] 3
          ["C" "Z"] 3
          :else 0)))

(defn points2 [[a b]]
  (+
   (get {"X" 0 "Y" 3 "Z" 6} b)
   (match [a b]
          ["A" "X"] 3
          ["A" "Y"] 1
          ["A" "Z"] 2
          ["B" "X"] 1
          ["B" "Y"] 2
          ["B" "Z"] 3
          ["C" "X"] 2
          ["C" "Y"] 3
          ["C" "Z"] 1
          :else 0)))

(->> rounds
     (map points2)
     (reduce +))

(def in3 (slurp "resources/input3"))

(defn a-z []
 (let [x(for [x (range 97 123)]
          (char x))
       y (for [x (range 65 91)]
           (char x))]
   (concat x y)))

(defn lookup-table []
  (->> (a-z)
       (map vector (range 1 55))
       (reduce (fn [acc [v k]] (assoc acc k v)) {})))

(require '[clojure.set :as set])

(defn rucksack-intersection
  [rucksack]
  (let [m (/ (count rucksack) 2)]
    (->> (seq rucksack)
         (split-at m)
         (map set)
         (apply set/intersection))))

(def lookup (lookup-table))

(->> in3
     (str/split-lines)
     (mapcat rucksack-intersection)
     (map (fn [x] (get lookup x)))
     (reduce +))

(->> in3
     (str/split-lines)
     (partition 3)
     (mapcat (fn [c] (apply set/intersection (map set c))))
     (map (fn [x] (get lookup x)))
     (reduce +))

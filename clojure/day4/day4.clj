(ns user
  (:require [clojure.string :as str]))

(def test-input
  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(def input (slurp "input.txt"))

(defn parse [s]
  (->> s
       (str/split-lines)
       (mapv #(->> % seq (map str) (into [])))))

(defn count-x-y [matrix]
  (let [x (-> matrix first count)
        y (-> matrix count)]
    [x y]))

(defn cartesian-product [[x y]]
  (into []
   (for [y' (range 0 y)
         x' (range 0 x)]
     [x' y'])))

(defn ->loopup [matrix x-ys]
  (reduce
   (fn [m [x y]]
     (assoc m [x y] (get-in matrix [y x]))) {} x-ys))

(defn adjacent-positions [x y]
  (->> (for [y' (range (dec y) (+ 2 y))
             x' (range (dec x) (+ 2 x))
             :when (not= [x y] [x' y'])]
         [x' y'])
       (remove (fn [[x y]] (or (neg? x) (neg? y))))))

(defn calc [s]
  (let [matrix (parse s)
        x-ys   (-> matrix count-x-y cartesian-product)
        lookup (->loopup matrix x-ys)]
    (->> x-ys
         (mapv (fn [[x y]]
                 (let [currenct (get lookup [x y])]
                   (if (= currenct ".")
                     [[x y] false]
                     (let [check-positions (adjacent-positions x y)
                           counts          (->> (mapv (fn [[x y]] (get lookup [x y])) check-positions)
                                                (filter #(= "@" %))
                                                count)]
                       [[x y] (< counts 4)])))))
         (filter second)
         (count))))

(calc test-input)
;; => 13

(calc input)
;; => 1416

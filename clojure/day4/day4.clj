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

(defn paper-around [lookup x y]
  (let [currenct (get lookup [x y])]
    (if (= currenct ".")
      [[x y] nil]
      (let [check-positions (adjacent-positions x y)
            counts          (->> (mapv (fn [[x y]] (get lookup [x y])) check-positions)
                                 (filter #(= "@" %))
                                 count)]
        [[x y] counts]))))

(defn calc [s]
  (let [matrix (parse s)
        x-ys   (-> matrix count-x-y cartesian-product)
        lookup (->loopup matrix x-ys)]
    (->> x-ys
         (mapv (fn [[x y]]
                 (paper-around lookup x y)))
         (remove (comp nil? second))
         (filter (fn [[_ s]] (< s 4)))
         (count))))

(calc test-input)
;; => 13

(calc input)
;; => 1416


;; --- Problem 2 ---

(defn next-generation-lookup [lookup to-remove]
  (reduce
   (fn [acc [k _]]
     (assoc acc k "."))
   lookup
   to-remove))

(defn ->to-remove [lookup x-ys]
  (->> x-ys
       (mapv (fn [[x y]]
               (paper-around lookup x y)))
       (remove (comp nil? second))
       (filter (fn [[_ s]] (< s 4)))))

(defn calc2 [in]
  (let [matrix (parse in)
        x-ys   (-> matrix count-x-y cartesian-product)]
    (loop [lookup  (->loopup matrix x-ys)
           counter 0]
      (let [to-remove (->to-remove lookup x-ys)]
      (if (empty? to-remove)
        counter
        (recur
         (next-generation-lookup lookup to-remove)
         (+ counter (count to-remove))))))))

(calc2 test-input)
;; => 43

(calc2 input)
;; => 9086

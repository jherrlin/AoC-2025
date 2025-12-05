(ns user
  (:require
   [clojure.string :as str]))

(def test-input
  "987654321111111
811111111111119
234234234234278
818181911112111")

(defn parse [s]
  (->> s
       str/split-lines
       (mapv #(mapv (fn [c] (Integer. (str c))) %))))

(defn calc [xs]
  (->> xs
       (mapv (fn [row]
               (->> (for [i (range (count row))
                          j (range (inc i) (count row))]
                      (Integer. (str (row i) (row j))))
                    (apply max))))
       (reduce +)))

(->> test-input
     parse
     calc)

(->> (slurp "input.txt")
     parse
     calc)
;; => 17316 is correct

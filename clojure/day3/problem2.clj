(ns user
  (:require [clojure.string :as str]))

(defn input []
  (->> "input.txt"
       slurp
       str/split-lines
       (map str/trim)))

(def test-input
  (->> "987654321111111
811111111111119
234234234234278
818181911112111"
       str/split-lines
       (map str/trim)))

(defn parse-bank [line]
  (mapv #(Integer/parseInt (str %)) line))

(defn solve [acc nb bank]
  (if (zero? nb)
    acc
    (let [split-index     (- (count bank) (dec nb))
          candidates      (take split-index bank)
          [next-idx max]  (->> (map-indexed vector candidates)
                               (sort-by second #(compare %2 %1))
                               first)
          remaining-banks (drop (inc next-idx) bank)]
      (recur (conj acc max) (dec nb) remaining-banks))))

(defn calc [xs]
  (->> xs
       (mapv parse-bank)
       (mapv #(solve [] 12 %))
       (map (fn [digits] (Long/parseLong (apply str digits))))
       (reduce +)))


(calc test-input)
;; => 3121910778619

(calc (input))
;; => 171741365473332

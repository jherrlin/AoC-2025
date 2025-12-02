(ns user
  (:require [clojure.string :as str]))

(def test-input
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")


(defn split-string [s]
  (let [mid (quot (count s) 2)]
    [(subs s 0 mid)
     (subs s mid)]))

(defn invalid? [s]
  (apply = (split-string s)))

(invalid? "1188511885")
(invalid? "1188511881")
(invalid? "101")

(defn parse [s]
  (->> (str/split s #",")
       (map str/trim)
       (map #(str/split % #"-"))
       (mapv (fn [lst] (mapv #(Long. %) lst)))
       (mapv (fn [[start end]] (range start (inc end))))
       (mapv (fn [lst] (mapv str lst)))
       (apply concat)))

(defn calc [xs]
  (->> xs
       (map (fn [s]
              (if (invalid? s)
                s
                false)))
       (filter string?)
       (map #(Long. %))
       (reduce +)))

;; test
(->> test-input
     parse
     calc)

(->> (slurp "input.txt")
     parse
     calc)
;; => 30323879646


;; === Problem 2 ===

(defn invalid2? [s]
  (or (invalid? s)
      (->> (reduce
            (fn [acc nr]
              (conj acc (apply = (partition-all nr s))))
            []
            (range 1 (count s)))
           (some true?))))

(invalid2? "11")
(invalid2? "12")

(defn calc2 [xs]
  (->> xs
       (map (fn [s]
              (if (invalid2? s)
                s
                false)))
       (filter string?)
       (map #(Long. %))
       (reduce +)))

(->> test-input
     parse
     calc2)
;; => 33986149340

(->> (slurp "input.txt")
     parse
     calc2)
;; 43872163557

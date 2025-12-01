(ns user)


(def test-input
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(def start-value 50)

(defn parse [s]
  (->> (clojure.string/split s #"\n")
       (map (fn [s']
              [(eval (symbol (str (first s'))))
               (Integer. (apply str (rest s')))]))))

(defn L [current v]
  (let [hundreds (quot v 100)
        rst      (rem v 100)
        v'       (- current rst)]
    (if-not (neg? v')
      v'
      (+ 100 v'))))

(defn R [current v]
  (let [hundreds (quot v 100)
        rst      (rem v 100)
        v'       (+ rst current)]
    (if-not (< 99 v')
      v'
      (abs (- 100 v')))))

(defn calc [xs]
  (reduce
   (fn [[counter acc] [operator value]]
     (let [acc' (operator acc value)]
       [(if (= 0 acc') (inc counter) counter)
        acc']))
   [0 start-value]
   xs))

(->> test-input
     parse
     calc)

(->> (slurp "input.txt")
     parse
     calc)
;; => [1177 68]
;; 1177 is correct


;; === Problem 2 ===

(defn q-r [v] [(quot v 100) (rem v 100)])

(defn bump-pred [pred current v]
  (and (not= current 0) (pred v) (not= v 0) (not= v 100)))

(defn L [current v]
  (let [[hundreds rst] (q-r v)
        v'             (- current rst)
        bump           (bump-pred neg? current v')]
    [(if bump (inc hundreds) hundreds)
     (if-not (neg? v')
       v'
       (+ 100 v'))]))

(defn R [current v]
  (let [[hundreds rst] (q-r v)
        v'             (+ rst current)
        bump           (bump-pred #(< 99 %) current v')]
    [(if bump (inc hundreds) hundreds)
     (if-not (< 99 v')
       v'
       (abs (- 100 v')))]))

(defn calc2 [xs]
  (reduce
   (fn [[counter acc] [operator value]]
     (let [[rem acc'] (operator acc value)]
       [(+ rem (if (= 0 acc') (inc counter) counter))
        acc']))
   [0 start-value]
   xs))

(-> test-input
    parse
    calc2)

(->> (slurp "input.txt")
     parse
     calc2)
;; => [6768 68]
;; 6768 is the correct

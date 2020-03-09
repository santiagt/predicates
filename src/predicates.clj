(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [x] (< x n)))

(defn equal-to [n]
  (fn [x] (== x n)))

(defn set->predicate [a-set]
  (fn [x] (contains? a-set x)))

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (let [check (fn [x] (or (empty? x) (nil? x) (every? whitespace? x)))]
    (check string)))

(defn has-award? [book award]
  (contains? (:awards book) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (fn [award] (has-award? book award)) awards))

(defn my-some [pred a-seq]
  (let [check (first (map pred (filter pred a-seq)))]
    (cond (nil? check) false
          (true? check) true
          :else check)))

(defn my-every? [pred a-seq]
  (empty? (filter #(not (pred %)) a-seq)))

(defn prime? [n]
  (let [pred (fn [x] (= (rem n x) 0))]
    (not (some pred (range 2 n)))))
;^^

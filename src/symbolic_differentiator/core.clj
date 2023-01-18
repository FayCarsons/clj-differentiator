(ns symbolic-differentiator.core
  (:use clojure.test)
  (:require [clojure.string :as s]
            [clojure.test :as t]))

(use 'symbolic-differentiator.core :reload)

(defn log [v]
  (println v)
  v)

(def parse-pair #(cond
                   (== (second %) 0) (str (first %))
                   (== (second %) 1) (str (first %) "x")
                   :else (str (first %) "x^" (second %))))

(defn stringify-pairs [pairs]
  (s/replace
   (->> pairs
        (filter some?)
        (map parse-pair)
        (s/join " + "))
   "+ -"
   "- "))

; anonymous functions that return matching string if present, nil otherwise
(def get-factor #(re-find #"^-?[0-9]+" %))
(def get-exponent #(re-find #"-?[0-9]+$" %))

(defn compute-derivative [[factor exponent]]
  [(* factor exponent) (dec exponent)])

; takes monomial string and returns factor and/or exponent if present
; nil otherwise
; discards constants 
(defn parse-monomial [monomial]
  (if-let [contant (parse-long monomial)]
    [contant 0]
   (let [factor (if-let [num (get-factor monomial)]
                  (parse-long num)
                  1)
         exponent (if-let [num (get-exponent monomial)]
                    (parse-long num)
                    1)]
     [factor exponent])))



; takes in raw unformatted polynomial string, splits into vector of strings, 
; parses strings into vector of factor + exponent, does differentiation,
; converts back to formatted string
(defn differentiate [expr]
  (let [monomials (-> expr 
                      (s/replace #"[-]" "+-")
                      (s/split #"[+]"))]
    (->> monomials
         (map #(s/replace % #" " ""))
         (remove #(or (parse-long %)
                      (s/blank? %)))
         (map parse-monomial)
         (map compute-derivative)
         (stringify-pairs))))

; testing
(deftest symbolic-differentiator
 (let [poly-vec ["-12x^3 + x^2 - 5x + 6"
                 "5x2+ 2x+7"
                 "6x^4 + 3x^3 + 3x^2 + 2x + 1"
                 "x^8 + x^5 + 3x + 10"
                 "5x - x^2 - 3x^7 - 10"
                 "12x^7 - 10x - 2x^3 - 1 - 23x^100"
                 "5 + 3 + 1 + 10 + 10000000 + 2x^3 - 3x^29 - 100 + 420x"]
       derivatives (map differentiate poly-vec)
       test-vec ["-36x^2 + 2x - 5"
                 "10x + 2"
                 "24x^3 + 9x^2 + 6x + 2"
                 "8x^7 + 5x^4 + 3"
                 "5 + 2x - 21x^6"
                 "84x^6 - 10 - 6x^2 - 2300x^99"
                 "6x^2 - 87x^28 + 420"]]
   (dotimes [n (count derivatives)]
     (is (= (nth derivatives n)
            (nth test-vec n))))))

(run-tests 'symbolic-differentiator.core)
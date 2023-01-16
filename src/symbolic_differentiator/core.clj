(ns symbolic-differentiator.core
  (:require [clojure.string :as s]))

(use 'symbolic-differentiator.core :reload)

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

; takes monomial string and returns factor and/or exponent if present, nil otherwise
; discards constants 
(defn parse-monomial [monomial]
  (when (> (count monomial) 1)
    (let [factor (if-let [num (get-factor monomial)]
                   (read-string num)
                   1)
          exponent (if-let [num (get-exponent monomial)]
                     (read-string num)
                     1)]
      [(* factor exponent) (dec exponent)])))

; takes in raw unformatted polynomial string, splits into vector of strings, 
; parses those strings into a vector of two element vectors, does differentiation,
; and converts back to formatted string
(defn deriv [expr]
  (let [monomials (-> expr
                      (s/replace #"[-]" "+-")
                      (s/split #"[+]"))
        var (re-matches #"[a-z]" expr)]
    (->> monomials
         (map #(s/replace % #" " ""))
         (map parse-monomial)
         (stringify-pairs))))

(defn main []
  (let [polynomial-string "-12x^3 + x^2 - 5x + 6"
        polynomial-string-two "5x2+ 2x+7"
        derivative (deriv polynomial-string)
        derivative-two (deriv polynomial-string-two)]
    (println derivative)
    (println derivative-two)))

(main)




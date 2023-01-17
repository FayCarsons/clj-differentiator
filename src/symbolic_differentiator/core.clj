(ns symbolic-differentiator.core
  (:use clojure.test)
  (:require [clojure.string :as s]
            [clojure.test :as t]))

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

; takes monomial string and returns factor and/or exponent if present
; nil otherwise
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
; parses strings into vector of factor + exponent, does differentiation,
; converts back to formatted string
(defn differeniate [expr]
  (let [monomials (-> expr 
                      (format)
                      (s/replace #"[-]" "+-")
                      (s/split #"[+]"))
        var (re-matches #"[a-z]" expr)]
    (->> monomials
         (map #(s/replace % #" " ""))
         (filter #(not (parse-long %)))
         (map parse-monomial)
         (stringify-pairs))))

; testing
(deftest symbolic-differentiator
 (let [poly-vec ["-12x^3 + x^2 - 5x + 6"
                 "5x2+ 2x+7"
                 "6x^4 + 3x^3 + 3x^2 + 2x + 1"
                 "x^8 + x^5 + 3x + 10"]
       derivatives (map differeniate poly-vec)
       test-vec ["-36x^2 + 2x - 5"
                 "10x + 2"
                 "24x^3 + 9x^2 + 6x + 2"
                 "8x^7 + 5x^4 + 3"]]
   (dotimes [n (count derivatives)]
     (is (= (nth derivatives n)
            (nth test-vec n))))))

(run-tests 'symbolic-differentiator.core)

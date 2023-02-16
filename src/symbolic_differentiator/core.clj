(ns symbolic-differentiator.core
  (:use clojure.test)
  (:require [clojure.string :as s] 
            [clojure.test :as t]))

(use 'symbolic-differentiator.core :reload)

(defn log
  "prints to repl and returns"
  [v]
  (println v)
  v)

; determines whether factor, exponent, or both should be present 
; and converts to string
(def parse-pair
  #(cond
     (== (second %) 0) (str (first %))
     (== (second %) 1) (str (first %) "x")
     :else (str (first %) "x^" (second %))))

(defn stringify-pairs
  "takes in 2 element vector, returns monomial string"
  [pairs]
  (s/replace
   (->> pairs
        (filter some?)
        (map parse-pair)
        (s/join " + "))
   "+ -"
   "- "))

;both return matching string if present, nil otherwise
(def get-factor #(re-find #"^-?[0-9]+" %))
(def get-exponent #(re-find #"-?[0-9]+$" %))

(defn parse-monomial
  "takes in monomial string and returns 2 element vector of factor + exponent, 
   or constant + 0"
  [monomial]
  (if-let [constant (parse-long monomial)]
    [constant 0]
    (let [factor (if-let [num (get-factor monomial)]
                   (parse-long num)
                   1)
          exponent (if-let [num (get-exponent monomial)]
                     (parse-long num)
                     1)]
      [factor exponent])))

(defn compute-derivative [[factor exponent]]
  [(* factor exponent) (dec exponent)])

(defn differentiate 
  "main - takes in polynomial string, does formatting parsing and 
   differentiation, then converts back to string"
  [expr]
  (let [monomials (-> expr 
                      (s/replace #"[-]" "+-")
                      (s/split #"[+]"))]
    (->> monomials
         (map #(s/replace % #" " ""))
         (remove #(or (s/blank? %)
                      (parse-long %)))
         (map parse-monomial)
         (map compute-derivative)
         (stringify-pairs))))

(do
  (deftest symbolic-differentiator
    (let [poly-vec ["-4x^2 - 4x^2 - 2x^4 - 6x^2 - 2x + 7x"
                    "6x^4 - 3x^3 + 8x^4 + 4x^4 - 2x^2 - 7x^4"
                    "1x^2 + 3x^3 + 3x^4"
                    "4x^4 + 7x - 7x - 4x^3 - 5x^2"]
          derivatives (map differentiate poly-vec) 
          test-vec ["8x^3 - 28x + 5"
                    "44x^3 - 9x^2 - 4x"
                    "12x^3 + 9x^2 + 2x"
                    "16x^3 - 12x^2 - 10x"]] 
      (dotimes [n (count derivatives)]
        (is (= (nth derivatives n)
               (nth test-vec n)))))) 

  (run-tests 'symbolic-differentiator.core))
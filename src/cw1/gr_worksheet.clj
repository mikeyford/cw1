;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns cw1_worksheet
  (:require [kixi.stats.random :as k]
            [clojure.core.matrix.operators :as m]
            [clojure.core.matrix.stats :as s]
            [gorilla-plot.core :as plt])
  (:use clojure.core.matrix))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn mean-squared-error [Y_hat Y]
  (let [n (count Y_hat)
        delta (map - Y_hat Y)
        delta_squared (map * delta delta)
        sum_delta_squared (reduce + delta_squared)
        ]
   		(/ sum_delta_squared n)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/mean-squared-error</span>","value":"#'cw1_worksheet/mean-squared-error"}
;; <=

;; @@
(defn solve-w [n_of_samps training_set_size]
  (let [w_actual (sample_norm)
    	x (vec (repeatedly n_of_samps #(sample_norm)))
    	n (vec (repeatedly n_of_samps #(sample_norm)))
    	y (m/+ (mmul (transpose x) w_actual) n)
        
        x_train (transpose (matrix [(nth (split-at training_set_size x) 0)]))
        y_train (transpose (matrix [(nth (split-at training_set_size y) 0)]))
        
        x_test (transpose (matrix [(nth (split-at training_set_size x) 1)]))
        y_test (transpose (matrix [(nth (split-at training_set_size y) 1)]))

        
    	w_estimated (mmul (mmul (inverse (mmul (transpose x_train) x_train)) (transpose x_train)) y_train)
        
        Y_hat_train (reduce into [] (mmul x_train w_estimated))
        Y_train (reduce into [] y_train)
        
        Y_hat_test (reduce into [] (mmul x_test w_estimated))
        Y_test (reduce into [] y_test)
        
        MSE_train (cw1_worksheet/mean-squared-error Y_hat_train Y_train)
        MSE_test (cw1_worksheet/mean-squared-error Y_hat_test Y_test)
        ]
    	{:MSE_train MSE_train :MSE_test MSE_test}
    )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/solve-w</span>","value":"#'cw1_worksheet/solve-w"}
;; <=

;; @@
(def train_100 (repeatedly 200 #(solve-w 600 100)))
(println ["Training set 100 average MSE:" (s/mean (into [] (map :MSE_train train_100))) "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_100)))])

;; @@
;; ->
;;; [Training set 100 average MSE: 1.0047358409721037 ; Test set 500 average MSE: 1.0108026741256257]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def train_10 (repeatedly 200 #(solve-w 510 10)))
(println ["Training set 10 average MSE:" (s/mean (into [] (map :MSE_train train_10))) "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_10)))])
;; @@
;; ->
;;; [Training set 10 average MSE: 0.8664354566385987 ; Test set 500 average MSE: 1.135714504086479]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@

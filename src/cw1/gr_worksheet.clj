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
  (:require ;[kixi.stats.random :as k]
            [clojure.core.matrix.operators :as m]
            [clojure.core.matrix.stats :as s]
            [clojure.core.matrix.random :as r]
            [gorilla-plot.core :as plt])
  (:use clojure.core.matrix))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;(defn sample_norm []
;  (k/draw (k/normal {:mu 0, :sd 1})))


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
(defn solve-w-Rn [n_of_dimensions n_of_samps training_set_size]
  (let [w (r/sample-normal [n_of_dimensions 1])
    	X (r/sample-normal [n_of_samps n_of_dimensions])
    	n (r/sample-normal [n_of_samps 1])
    	y (m/+ (mmul X w) n)
        
        X_train (submatrix X 0 training_set_size 0 n_of_dimensions)
        y_train (submatrix y 0 training_set_size 0 1)
        
        
        X_test (submatrix X training_set_size (- n_of_samps training_set_size) 0 n_of_dimensions)
        y_test (submatrix y training_set_size (- n_of_samps training_set_size) 0 1)
        
        
        w_estimated (mmul (inverse (mmul (transpose X_train) X_train)) (transpose X_train) y_train)
        
        Y_hat_train (reduce into [] (mmul X_train w_estimated))
        Y_train (reduce into [] y_train)
        
        Y_hat_test (reduce into [] (mmul X_test w_estimated))
        Y_test (reduce into [] y_test)
        
        MSE_train (cw1_worksheet/mean-squared-error Y_hat_train Y_train)
        MSE_test (cw1_worksheet/mean-squared-error Y_hat_test Y_test)
        ]    	
    	{:MSE_train MSE_train :MSE_test MSE_test}
          
    )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/solve-w-Rn</span>","value":"#'cw1_worksheet/solve-w-Rn"}
;; <=

;; @@
;R1 - training set of 100
(def train_100 (repeatedly 200 #(solve-w-Rn 1 600 100)))
(print ["Training set 100 average MSE:" (s/mean (into [] (map :MSE_train train_100))) "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_100)))])
;; @@
;; ->
;;; [Training set 100 average MSE: 0.9839982829491721 ; Test set 500 average MSE: 1.0100376206820227]
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;R1 - training set of 100
(def train_10 (repeatedly 200 #(solve-w-Rn 1 510 10)))
(println ["Training set 10 average MSE:" (s/mean (into [] (map :MSE_train train_10))) "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_10)))])
;; @@
;; ->
;;; [Training set 10 average MSE: 0.9329119304333404 ; Test set 500 average MSE: 1.1049274597474106]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;R10 - training set of 100
(def train_100 (repeatedly 200 #(solve-w-Rn 10 600 100)))
(println ["Training set 100 average MSE:" (s/mean (into [] (map :MSE_train train_100))) "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_100)))])
;; @@
;; ->
;;; [Training set 100 average MSE: 0.9078532569519144 ; Test set 500 average MSE: 1.1206457672549677]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;R10 - training set of 10
(def train_10 (repeatedly 200 #(solve-w-Rn 10 510 10)))
(println ["Training set 10 average MSE:" (s/mean (into [] (map :MSE_train train_10))) "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_10)))])

;; @@
;; ->
;;; [Training set 10 average MSE: 5.535036492403341E-23 ; Test set 500 average MSE: 527.8125651726323]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@

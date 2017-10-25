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
            [gorilla-plot.core :as plt])
  (:use clojure.core.matrix))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def n_of_samps 600)
(def training_set_size 500)

(defn sample_norm []
  (k/draw (k/normal {:mu 0, :sd 1})))

(def w (sample_norm))
(def x (vec (repeatedly n_of_samps #(sample_norm))))
(def n (vec (repeatedly n_of_samps #(sample_norm))))

(def y (m/+ (mmul (transpose x) w) n))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/y</span>","value":"#'cw1_worksheet/y"}
;; <=

;; @@
x

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>1.5316079586225684</span>","value":"1.5316079586225684"},{"type":"html","content":"<span class='clj-double'>1.159966779586114</span>","value":"1.159966779586114"},{"type":"html","content":"<span class='clj-double'>0.5707239916307651</span>","value":"0.5707239916307651"},{"type":"html","content":"<span class='clj-double'>0.8487987951290046</span>","value":"0.8487987951290046"},{"type":"html","content":"<span class='clj-double'>1.2116351349177714</span>","value":"1.2116351349177714"},{"type":"html","content":"<span class='clj-double'>1.4970145414884597</span>","value":"1.4970145414884597"},{"type":"html","content":"<span class='clj-double'>1.375636780356435</span>","value":"1.375636780356435"},{"type":"html","content":"<span class='clj-double'>0.9206076868134131</span>","value":"0.9206076868134131"},{"type":"html","content":"<span class='clj-double'>-0.3552768173542019</span>","value":"-0.3552768173542019"},{"type":"html","content":"<span class='clj-double'>1.4570582054938037</span>","value":"1.4570582054938037"}],"value":"[1.5316079586225684 1.159966779586114 0.5707239916307651 0.8487987951290046 1.2116351349177714 1.4970145414884597 1.375636780356435 0.9206076868134131 -0.3552768173542019 1.4570582054938037]"}
;; <=

;; @@
w
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.11926689356693015</span>","value":"0.11926689356693015"}
;; <=

;; @@
(defn estimate-w [n_of_samps training_set_size]
  (let [w_actual (sample_norm)
    	x (vec (repeatedly n_of_samps #(sample_norm)))
    	n (vec (repeatedly n_of_samps #(sample_norm)))
    	y (m/+ (mmul (transpose x) w_actual) n)
        
        x_train (transpose (matrix [(nth (split-at training_set_size x) 0)]))
        y_train (transpose (matrix [(nth (split-at training_set_size y) 0)]))

        
    	w_estimated (mmul (mmul (inverse (mmul (transpose x_train) x_train)) (transpose x_train)) y_train)]
        (m/- w_actual w_estimated)))

(estimate-w 6000 5500)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.0011938388733179206</span>","value":"0.0011938388733179206"}],"value":"[0.0011938388733179206]"}],"value":"[[0.0011938388733179206]]"}
;; <=

;; @@

(def y_train (transpose (matrix [(nth (split-at training_set_size y) 0)])))
(def y_test (transpose (matrix [(nth (split-at training_set_size y) 1)])))
  
(def x_train (transpose (matrix [(nth (split-at training_set_size x) 0)])))
(def x_test (transpose (matrix [(nth (split-at training_set_size x) 1)])))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/x_test</span>","value":"#'cw1_worksheet/x_test"}
;; <=

;; @@
(defn solve_for_w [x y]
  " w = (X'X)^-1.X'.Y "
  (mmul (mmul (inverse (mmul (transpose x) x)) (transpose x)) y))

;(mmul (mmul (inverse (mmul (transpose x_train) x_train)) (transpose x_train)) y_train) 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/solve_for_w</span>","value":"#'cw1_worksheet/solve_for_w"}
;; <=

;; @@
(def w_estimated (solve_for_w x_train y_train))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/w_estimated</span>","value":"#'cw1_worksheet/w_estimated"}
;; <=

;; @@
w
(m/- w w_estimated)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>-0.006581439084299978</span>","value":"-0.006581439084299978"}],"value":"[-0.006581439084299978]"}],"value":"[[-0.006581439084299978]]"}
;; <=

;; @@
(defn generate-vals [n n_training_set]
  {:n n :n_train n_training_set :w (sample_norm)})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/generate-vals</span>","value":"#'cw1_worksheet/generate-vals"}
;; <=

;; @@
(defn calc-sqr-map [x]
  { :x (matrix [x x])  :y (* x x) })

(calc-sqr-map 3)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[3 3]"}],"value":"[:x [3 3]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}],"value":"[:y 9]"}],"value":"{:x [3 3], :y 9}"}
;; <=

;; @@
(def v (generate-map 10 5))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/v</span>","value":"#'cw1_worksheet/v"}
;; <=

;; @@
v
;; @@

;; @@

;; @@

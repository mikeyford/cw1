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
            [gorilla-plot.core :as plt]
    		[gg4clj.core :as gg4clj])
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
(print ["Training set 100 average MSE:" (s/mean (into [] (map :MSE_train train_100))) 
        "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_100)))])
;; @@
;; ->
;;; [Training set 100 average MSE: 0.9839982829491721 ; Test set 500 average MSE: 1.0100376206820227]
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;R1 - training set of 10
(def train_10 (repeatedly 200 #(solve-w-Rn 1 510 10)))
(println ["Training set 10 average MSE:" (s/mean (into [] (map :MSE_train train_10))) 
          "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_10)))])
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
(println ["Training set 100 average MSE:" (s/mean (into [] (map :MSE_train train_100))) 
          "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_100)))])
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
(println ["Training set 10 average MSE:" (s/mean (into [] (map :MSE_train train_10))) 
          "; Test set 500 average MSE:"  (s/mean (into [] (map :MSE_test train_10)))])

;; @@
;; ->
;;; [Training set 10 average MSE: 5.535036492403341E-23 ; Test set 500 average MSE: 527.8125651726323]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ##Exercise 4
;; **

;; @@
(defn solve-RR-w [n_of_dimensions n_of_samps training_set_size gamma]
  (let [w (r/sample-normal [n_of_dimensions 1])
    	X (r/sample-normal [n_of_samps n_of_dimensions])
    	n (r/sample-normal [n_of_samps 1])
    	y (m/+ (mmul X w) n)
        
        X_train (submatrix X 0 training_set_size 0 n_of_dimensions)
        y_train (submatrix y 0 training_set_size 0 1)
        
       
        X_test (submatrix X training_set_size (- n_of_samps training_set_size) 0 n_of_dimensions)
        y_test (submatrix y training_set_size (- n_of_samps training_set_size) 0 1)
        
        inv (m/+ (mmul (transpose X_train) X_train) 
                 (mmul gamma n_of_dimensions (identity-matrix n_of_dimensions)))   
        
        w_estimated (mmul (inverse inv) (transpose X_train) y_train)
        
        Y_hat_train (reduce into [] (mmul X_train w_estimated))
        Y_train (reduce into [] y_train)
        
        Y_hat_test (reduce into [] (mmul X_test w_estimated))
        Y_test (reduce into [] y_test)
        
        MSE_train (cw1_worksheet/mean-squared-error Y_hat_train Y_train)
        MSE_test (cw1_worksheet/mean-squared-error Y_hat_test Y_test)
        ]    	
    	{:MSE_train MSE_train :MSE_test MSE_test :gamma gamma}
          
    )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/solve-RR-w</span>","value":"#'cw1_worksheet/solve-RR-w"}
;; <=

;; **
;;; ##10-D Ridge Regresssion with varying gamma
;; **

;; **
;;; ###1 iteration with 100 training sample, 500 test samples 
;; **

;; @@
(def gamma_vals [])
(def MSE_train_vals [])
(def MSE_test_vals [])


(for [i [2E-6 2E-5 2E-4 2E-3 2E-2 0.2 2.0 20.0 200.0]] 
  (let [result (repeatedly 1 #(solve-RR-w 10 600 100 i))] 
         
       	(def gamma_vals (conj gamma_vals i))
        (def MSE_train_vals (conj MSE_train_vals (s/mean (into [] (map :MSE_train result)))))
        (def MSE_test_vals (conj MSE_test_vals (s/mean (into [] (map :MSE_test result)))))
   )
 )


(def dat {:gamma gamma_vals :MSE_train MSE_train_vals})

(gg4clj/view [[:<- :g (gg4clj/data-frame dat)]
              (gg4clj/r+
                [:ggplot :g [:aes :gamma :MSE_train]]
                [:geom_line {:colour "steelblue" :size 1}]
                [:geom_point {:colour "steelblue" :size 1}]
                [:scale_x_log10]
                [:scale_y_log10]
                )]
             {:width 8 :height 5})


(def dat {:gamma gamma_vals :MSE_test MSE_test_vals})

(gg4clj/view [[:<- :g (gg4clj/data-frame dat)]
              (gg4clj/r+
                [:ggplot :g [:aes :gamma :MSE_test]]
                [:geom_line {:colour "steelblue" :size 1}]
                [:geom_point {:colour "steelblue" :size 1}]
                [:scale_x_log10]
                [:scale_y_log10]
                )]
             {:width 8 :height 5})
;; @@
;; =>
;;; {"type":"html","content":"<?xml version='1.0' encoding='UTF-8'?>\n<svg viewBox='0 0 576.00 360.00' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns='http://www.w3.org/2000/svg'>\n<defs>\n<style type='text/css'>\n\n    line, polyline, path, rect, circle {\n      fill: none;\n      stroke: #000000;\n      stroke-linecap: round;\n      stroke-linejoin: round;\n      stroke-miterlimit: 10.00;\n    }\n  \n</style>\n</defs>\n<rect style='stroke: none; fill: #FFFFFF;' height='100%' width='100%'/>\n<rect style='stroke-width: 1.07; stroke: #FFFFFF; fill: #FFFFFF;' height='360.00' width='576.00' y='0.00' x='0.00'/>\n<defs>\n<clipPath id='d21bbb01-4a07-47d3-b157-ece8c3cf9f9d'>\n<rect height='321.79' width='534.35' y='5.48' x='36.17'/>\n</clipPath>\n</defs>\n<rect clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 1.07; stroke: none; fill: #EBEBEB;' height='321.79' width='534.35' y='5.48' x='36.17'/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='36.17,183.65 570.52,183.65 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='72.54,327.27 72.54,5.48 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='254.70,327.27 254.70,5.48 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='436.87,327.27 436.87,5.48 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='36.17,319.94 570.52,319.94 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='36.17,47.36 570.52,47.36 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='163.62,327.27 163.62,5.48 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='345.79,327.27 345.79,5.48 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='527.95,327.27 527.95,5.48 '/>\n<polyline clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 2.13; stroke: #4682B4; stroke-linecap: butt;' points='60.46,308.67 121.18,304.73 181.90,307.61 242.62,312.64 303.34,311.25 364.07,308.34 424.79,264.67 485.51,94.25 546.23,20.11 '/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='308.67' cx='60.46'/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='304.73' cx='121.18'/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='307.61' cx='181.90'/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='312.64' cx='242.62'/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='311.25' cx='303.34'/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='308.34' cx='364.07'/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='264.67' cx='424.79'/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='94.25' cx='485.51'/>\n<circle clip-path='url(#d21bbb01-4a07-47d3-b157-ece8c3cf9f9d)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='20.11' cx='546.23'/>\n<defs>\n<clipPath id='872b9fb2-a629-4f13-8e47-0be399984377'>\n<rect height='360.00' width='576.00' y='0.00' x='0.00'/>\n</clipPath>\n</defs>\n<g clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='5.01px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='323.16' x='26.23'>\n1\n</text>\n</g>\n<g clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='10.01px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='50.58' x='21.22'>\n10\n</text>\n</g>\n<polyline clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='33.43,319.94 36.17,319.94 '/>\n<polyline clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='33.43,47.36 36.17,47.36 '/>\n<polyline clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='163.62,330.01 163.62,327.27 '/>\n<polyline clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='345.79,330.01 345.79,327.27 '/>\n<polyline clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='527.95,330.01 527.95,327.27 '/>\n<g clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='23.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='152.11'>\n1e-04\n</text>\n</g>\n<g clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='23.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='334.28'>\n1e-01\n</text>\n</g>\n<g clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='25.28px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='515.31'>\n1e+02\n</text>\n</g>\n<g clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='36.68px' style='font-size: 11.00px; font-family: Arial;' y='351.99' x='285.00'>\ngamma\n</text>\n</g>\n<g clip-path='url(#872b9fb2-a629-4f13-8e47-0be399984377)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='47.68px' style='font-size: 11.00px; font-family: Arial;' transform='translate(13.35,190.21) rotate(-90)'>\nMSE_test\n</text>\n</g>\n</svg>\n","value":"#gg4clj.core.GGView{:plot-command [[:<- :g [:data.frame {:MSE_test [:c 1.099867751394207 1.1370778579360978 1.109744252914844 1.0635946086888755 1.0761525023035177 1.1029422007638714 1.5949548774344235 6.72944736156743 12.58845328692732], :gamma [:c 2.0E-6 2.0E-5 2.0E-4 0.002 0.02 0.2 2.0 20.0 200.0]}]] [:+ [:+ [:+ [:+ [:ggplot :g [:aes :gamma :MSE_test]] [:geom_line {:colour \"steelblue\", :size 1}]] [:geom_point {:colour \"steelblue\", :size 1}]] [:scale_x_log10]] [:scale_y_log10]]], :options {:width 8, :height 5}}"}
;; <=

;; **
;;; ###1 run with training sample of 10, test sample of 500
;; **

;; @@
(def gamma_vals [])
(def MSE_train_vals [])
(def MSE_test_vals [])


(for [i [2E-6 2E-5 2E-4 2E-3 2E-2 0.2 2.0 20.0 200.0]] 
  (let [result (repeatedly 1 #(solve-RR-w 10 510 10 i))] 
         
       	(def gamma_vals (conj gamma_vals i))
        (def MSE_train_vals (conj MSE_train_vals (s/mean (into [] (map :MSE_train result)))))
        (def MSE_test_vals (conj MSE_test_vals (s/mean (into [] (map :MSE_test result)))))
   )
 )


(def dat {:gamma gamma_vals :MSE_train MSE_train_vals})

(gg4clj/view [[:<- :g (gg4clj/data-frame dat)]
              (gg4clj/r+
                [:ggplot :g [:aes :gamma :MSE_train]]
                [:geom_line {:colour "steelblue" :size 1}]
                [:geom_point {:colour "steelblue" :size 1}]
                [:scale_x_log10]
                [:scale_y_log10]
                )]
             {:width 8 :height 5})


(def dat {:gamma gamma_vals :MSE_test MSE_test_vals})

(gg4clj/view [[:<- :g (gg4clj/data-frame dat)]
              (gg4clj/r+
                [:ggplot :g [:aes :gamma :MSE_test]]
                [:geom_line {:colour "steelblue" :size 1}]
                [:geom_point {:colour "steelblue" :size 1}]
                [:scale_x_log10]
                [:scale_y_log10]
                )]
             {:width 8 :height 5})
;; @@
;; =>
;;; {"type":"html","content":"<?xml version='1.0' encoding='UTF-8'?>\n<svg viewBox='0 0 576.00 360.00' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns='http://www.w3.org/2000/svg'>\n<defs>\n<style type='text/css'>\n\n    line, polyline, path, rect, circle {\n      fill: none;\n      stroke: #000000;\n      stroke-linecap: round;\n      stroke-linejoin: round;\n      stroke-miterlimit: 10.00;\n    }\n  \n</style>\n</defs>\n<rect style='stroke: none; fill: #FFFFFF;' height='100%' width='100%'/>\n<rect style='stroke-width: 1.07; stroke: #FFFFFF; fill: #FFFFFF;' height='360.00' width='576.00' y='0.00' x='0.00'/>\n<defs>\n<clipPath id='80ddc75d-2f2f-4451-b008-e58d2baadb3a'>\n<rect height='321.79' width='529.35' y='5.48' x='41.17'/>\n</clipPath>\n</defs>\n<rect clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 1.07; stroke: none; fill: #EBEBEB;' height='321.79' width='529.35' y='5.48' x='41.17'/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,277.14 570.52,277.14 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,120.25 570.52,120.25 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='77.20,327.27 77.20,5.48 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='257.66,327.27 257.66,5.48 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='438.12,327.27 438.12,5.48 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,198.70 570.52,198.70 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,41.80 570.52,41.80 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='167.43,327.27 167.43,5.48 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='347.89,327.27 347.89,5.48 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='528.35,327.27 528.35,5.48 '/>\n<polyline clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 2.13; stroke: #4682B4; stroke-linecap: butt;' points='65.23,183.78 125.39,223.36 185.54,20.11 245.69,312.64 305.85,274.42 366.00,287.07 426.15,236.74 486.31,176.42 546.46,208.35 '/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='183.78' cx='65.23'/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='223.36' cx='125.39'/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='20.11' cx='185.54'/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='312.64' cx='245.69'/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='274.42' cx='305.85'/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='287.07' cx='366.00'/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='236.74' cx='426.15'/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='176.42' cx='486.31'/>\n<circle clip-path='url(#80ddc75d-2f2f-4451-b008-e58d2baadb3a)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='208.35' cx='546.46'/>\n<defs>\n<clipPath id='215ae738-9cef-4c91-a111-1f21a1394eb9'>\n<rect height='360.00' width='576.00' y='0.00' x='0.00'/>\n</clipPath>\n</defs>\n<g clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='10.01px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='201.91' x='26.23'>\n10\n</text>\n</g>\n<g clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='15.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='45.02' x='21.22'>\n100\n</text>\n</g>\n<polyline clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='38.43,198.70 41.17,198.70 '/>\n<polyline clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='38.43,41.80 41.17,41.80 '/>\n<polyline clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='167.43,330.01 167.43,327.27 '/>\n<polyline clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='347.89,330.01 347.89,327.27 '/>\n<polyline clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='528.35,330.01 528.35,327.27 '/>\n<g clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='23.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='155.92'>\n1e-04\n</text>\n</g>\n<g clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='23.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='336.38'>\n1e-01\n</text>\n</g>\n<g clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='25.28px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='515.71'>\n1e+02\n</text>\n</g>\n<g clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='36.68px' style='font-size: 11.00px; font-family: Arial;' y='351.99' x='287.51'>\ngamma\n</text>\n</g>\n<g clip-path='url(#215ae738-9cef-4c91-a111-1f21a1394eb9)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='47.68px' style='font-size: 11.00px; font-family: Arial;' transform='translate(13.35,190.21) rotate(-90)'>\nMSE_test\n</text>\n</g>\n</svg>\n","value":"#gg4clj.core.GGView{:plot-command [[:<- :g [:data.frame {:MSE_test [:c 12.4464846283085 6.963407001568568 137.4900575364946 1.8783022517356882 3.29148408801933 2.733574587466783 5.721786900931618 13.867262789007722 8.679336934325828], :gamma [:c 2.0E-6 2.0E-5 2.0E-4 0.002 0.02 0.2 2.0 20.0 200.0]}]] [:+ [:+ [:+ [:+ [:ggplot :g [:aes :gamma :MSE_test]] [:geom_line {:colour \"steelblue\", :size 1}]] [:geom_point {:colour \"steelblue\", :size 1}]] [:scale_x_log10]] [:scale_y_log10]]], :options {:width 8, :height 5}}"}
;; <=

;; **
;;; ###Average of 200 runs, with 100 training samples, 500 test samples
;; **

;; @@
(def gamma_vals [])
(def MSE_train_vals [])
(def MSE_test_vals [])


(for [i [2E-6 2E-5 2E-4 2E-3 2E-2 0.2 2.0 20.0 200.0]] 
  (let [result (repeatedly 200 #(solve-RR-w 10 600 100 i))] 
         
       	(def gamma_vals (conj gamma_vals i))
        (def MSE_train_vals (conj MSE_train_vals (s/mean (into [] (map :MSE_train result)))))
        (def MSE_test_vals (conj MSE_test_vals (s/mean (into [] (map :MSE_test result)))))
   )
 )


(def dat {:gamma gamma_vals :MSE_train MSE_train_vals})

(gg4clj/view [[:<- :g (gg4clj/data-frame dat)]
              (gg4clj/r+
                [:ggplot :g [:aes :gamma :MSE_train]]
                [:geom_line {:colour "steelblue" :size 1}]
                [:geom_point {:colour "steelblue" :size 1}]
                [:scale_x_log10]
                [:scale_y_log10]
                )]
             {:width 8 :height 5})


(def dat {:gamma gamma_vals :MSE_test MSE_test_vals})

(gg4clj/view [[:<- :g (gg4clj/data-frame dat)]
              (gg4clj/r+
                [:ggplot :g [:aes :gamma :MSE_test]]
                [:geom_line {:colour "steelblue" :size 1}]
                [:geom_point {:colour "steelblue" :size 1}]
                [:scale_x_log10]
                [:scale_y_log10]
                )]
             {:width 8 :height 5})

;; @@
;; =>
;;; {"type":"html","content":"<?xml version='1.0' encoding='UTF-8'?>\n<svg viewBox='0 0 576.00 360.00' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns='http://www.w3.org/2000/svg'>\n<defs>\n<style type='text/css'>\n\n    line, polyline, path, rect, circle {\n      fill: none;\n      stroke: #000000;\n      stroke-linecap: round;\n      stroke-linejoin: round;\n      stroke-miterlimit: 10.00;\n    }\n  \n</style>\n</defs>\n<rect style='stroke: none; fill: #FFFFFF;' height='100%' width='100%'/>\n<rect style='stroke-width: 1.07; stroke: #FFFFFF; fill: #FFFFFF;' height='360.00' width='576.00' y='0.00' x='0.00'/>\n<defs>\n<clipPath id='0c13d751-d49e-41a1-9c5d-a5860042f577'>\n<rect height='321.79' width='534.35' y='5.48' x='36.17'/>\n</clipPath>\n</defs>\n<rect clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 1.07; stroke: none; fill: #EBEBEB;' height='321.79' width='534.35' y='5.48' x='36.17'/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='36.17,170.99 570.52,170.99 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='72.54,327.27 72.54,5.48 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='254.70,327.27 254.70,5.48 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='436.87,327.27 436.87,5.48 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='36.17,326.72 570.52,326.72 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='36.17,15.25 570.52,15.25 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='163.62,327.27 163.62,5.48 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='345.79,327.27 345.79,5.48 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='527.95,327.27 527.95,5.48 '/>\n<polyline clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 2.13; stroke: #4682B4; stroke-linecap: butt;' points='60.46,310.71 121.18,310.70 181.90,312.64 242.62,311.99 303.34,312.06 364.07,312.36 424.79,278.66 485.51,96.32 546.23,20.11 '/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='310.71' cx='60.46'/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='310.70' cx='121.18'/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='312.64' cx='181.90'/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='311.99' cx='242.62'/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='312.06' cx='303.34'/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='312.36' cx='364.07'/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='278.66' cx='424.79'/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='96.32' cx='485.51'/>\n<circle clip-path='url(#0c13d751-d49e-41a1-9c5d-a5860042f577)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='20.11' cx='546.23'/>\n<defs>\n<clipPath id='5ba44151-a201-40db-aac2-49bfb9dd049c'>\n<rect height='360.00' width='576.00' y='0.00' x='0.00'/>\n</clipPath>\n</defs>\n<g clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='5.01px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='329.94' x='26.23'>\n1\n</text>\n</g>\n<g clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='10.01px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='18.47' x='21.22'>\n10\n</text>\n</g>\n<polyline clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='33.43,326.72 36.17,326.72 '/>\n<polyline clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='33.43,15.25 36.17,15.25 '/>\n<polyline clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='163.62,330.01 163.62,327.27 '/>\n<polyline clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='345.79,330.01 345.79,327.27 '/>\n<polyline clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='527.95,330.01 527.95,327.27 '/>\n<g clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='23.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='152.11'>\n1e-04\n</text>\n</g>\n<g clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='23.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='334.28'>\n1e-01\n</text>\n</g>\n<g clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='25.28px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='515.31'>\n1e+02\n</text>\n</g>\n<g clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='36.68px' style='font-size: 11.00px; font-family: Arial;' y='351.99' x='285.00'>\ngamma\n</text>\n</g>\n<g clip-path='url(#5ba44151-a201-40db-aac2-49bfb9dd049c)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='47.68px' style='font-size: 11.00px; font-family: Arial;' transform='translate(13.35,190.21) rotate(-90)'>\nMSE_test\n</text>\n</g>\n</svg>\n","value":"#gg4clj.core.GGView{:plot-command [[:<- :g [:data.frame {:MSE_test [:c 1.1256317998452077 1.1257751074056994 1.1097158708563948 1.115095042096392 1.1144667129437746 1.112032564730304 1.4265866126038358 5.491838255383078 9.647643532475115], :gamma [:c 2.0E-6 2.0E-5 2.0E-4 0.002 0.02 0.2 2.0 20.0 200.0]}]] [:+ [:+ [:+ [:+ [:ggplot :g [:aes :gamma :MSE_test]] [:geom_line {:colour \"steelblue\", :size 1}]] [:geom_point {:colour \"steelblue\", :size 1}]] [:scale_x_log10]] [:scale_y_log10]]], :options {:width 8, :height 5}}"}
;; <=

;; **
;;; ###Average of 200 runs, with 10 training samples, 500 test samples
;; **

;; @@
(def gamma_vals [])
(def MSE_train_vals [])
(def MSE_test_vals [])


(for [i [2E-6 2E-5 2E-4 2E-3 2E-2 0.2 2.0 20.0 200.0]] 
  (let [result (repeatedly 200 #(solve-RR-w 10 510 10 i))] 
         
       	(def gamma_vals (conj gamma_vals i))
        (def MSE_train_vals (conj MSE_train_vals (s/mean (into [] (map :MSE_train result)))))
        (def MSE_test_vals (conj MSE_test_vals (s/mean (into [] (map :MSE_test result)))))
   )
 )


(def dat {:gamma gamma_vals :MSE_train MSE_train_vals})

(gg4clj/view [[:<- :g (gg4clj/data-frame dat)]
              (gg4clj/r+
                [:ggplot :g [:aes :gamma :MSE_train]]
                [:geom_line {:colour "steelblue" :size 1}]
                [:geom_point {:colour "steelblue" :size 1}]
                [:scale_x_log10]
                [:scale_y_log10]
                )]
             {:width 8 :height 5})


(def dat {:gamma gamma_vals :MSE_test MSE_test_vals})

(gg4clj/view [[:<- :g (gg4clj/data-frame dat)]
              (gg4clj/r+
                [:ggplot :g [:aes :gamma :MSE_test]]
                [:geom_line {:colour "steelblue" :size 1}]
                [:geom_point {:colour "steelblue" :size 1}]
                [:scale_x_log10]
                [:scale_y_log10]
                )]
             {:width 8 :height 5})

;; @@
;; =>
;;; {"type":"html","content":"<?xml version='1.0' encoding='UTF-8'?>\n<svg viewBox='0 0 576.00 360.00' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns='http://www.w3.org/2000/svg'>\n<defs>\n<style type='text/css'>\n\n    line, polyline, path, rect, circle {\n      fill: none;\n      stroke: #000000;\n      stroke-linecap: round;\n      stroke-linejoin: round;\n      stroke-miterlimit: 10.00;\n    }\n  \n</style>\n</defs>\n<rect style='stroke: none; fill: #FFFFFF;' height='100%' width='100%'/>\n<rect style='stroke-width: 1.07; stroke: #FFFFFF; fill: #FFFFFF;' height='360.00' width='576.00' y='0.00' x='0.00'/>\n<defs>\n<clipPath id='3039d97a-c076-4a7f-90e8-5c5a427625f5'>\n<rect height='321.79' width='529.35' y='5.48' x='41.17'/>\n</clipPath>\n</defs>\n<rect clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 1.07; stroke: none; fill: #EBEBEB;' height='321.79' width='529.35' y='5.48' x='41.17'/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,326.06 570.52,326.06 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,190.96 570.52,190.96 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,55.86 570.52,55.86 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='77.20,327.27 77.20,5.48 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='257.66,327.27 257.66,5.48 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.53; stroke: #FFFFFF; stroke-linecap: butt;' points='438.12,327.27 438.12,5.48 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,258.51 570.52,258.51 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='41.17,123.41 570.52,123.41 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='167.43,327.27 167.43,5.48 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='347.89,327.27 347.89,5.48 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 1.07; stroke: #FFFFFF; stroke-linecap: butt;' points='528.35,327.27 528.35,5.48 '/>\n<polyline clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 2.13; stroke: #4682B4; stroke-linecap: butt;' points='65.23,20.11 125.39,99.89 185.54,171.16 245.69,240.04 305.85,300.83 366.00,312.64 426.15,279.62 486.31,258.18 546.46,253.96 '/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='20.11' cx='65.23'/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='99.89' cx='125.39'/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='171.16' cx='185.54'/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='240.04' cx='245.69'/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='300.83' cx='305.85'/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='312.64' cx='366.00'/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='279.62' cx='426.15'/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='258.18' cx='486.31'/>\n<circle clip-path='url(#3039d97a-c076-4a7f-90e8-5c5a427625f5)' style='stroke-width: 0.71; stroke: #4682B4; fill: #4682B4;' r='1.42pt' cy='253.96' cx='546.46'/>\n<defs>\n<clipPath id='adbeb5c5-9112-4595-99d1-77ea5efa34ae'>\n<rect height='360.00' width='576.00' y='0.00' x='0.00'/>\n</clipPath>\n</defs>\n<g clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='10.01px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='261.73' x='26.23'>\n10\n</text>\n</g>\n<g clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='15.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='126.63' x='21.22'>\n100\n</text>\n</g>\n<polyline clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='38.43,258.51 41.17,258.51 '/>\n<polyline clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='38.43,123.41 41.17,123.41 '/>\n<polyline clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='167.43,330.01 167.43,327.27 '/>\n<polyline clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='347.89,330.01 347.89,327.27 '/>\n<polyline clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' points='528.35,330.01 528.35,327.27 '/>\n<g clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='23.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='155.92'>\n1e-04\n</text>\n</g>\n<g clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='23.02px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='336.38'>\n1e-01\n</text>\n</g>\n<g clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='25.28px' style='font-size: 8.80px; fill: #4D4D4D; font-family: Arial;' y='338.63' x='515.71'>\n1e+02\n</text>\n</g>\n<g clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='36.68px' style='font-size: 11.00px; font-family: Arial;' y='351.99' x='287.51'>\ngamma\n</text>\n</g>\n<g clip-path='url(#adbeb5c5-9112-4595-99d1-77ea5efa34ae)'>\n<text lengthAdjust='spacingAndGlyphs' textLength='47.68px' style='font-size: 11.00px; font-family: Arial;' transform='translate(13.35,190.21) rotate(-90)'>\nMSE_test\n</text>\n</g>\n</svg>\n","value":"#gg4clj.core.GGView{:plot-command [[:<- :g [:data.frame {:MSE_test [:c 581.6533338335992 149.3028206142586 44.3174743441859 13.700647069403027 4.861107070626411 3.9749682338960266 6.97861540596207 10.055849972146857 10.806649286545891], :gamma [:c 2.0E-6 2.0E-5 2.0E-4 0.002 0.02 0.2 2.0 20.0 200.0]}]] [:+ [:+ [:+ [:+ [:ggplot :g [:aes :gamma :MSE_test]] [:geom_line {:colour \"steelblue\", :size 1}]] [:geom_point {:colour \"steelblue\", :size 1}]] [:scale_x_log10]] [:scale_y_log10]]], :options {:width 8, :height 5}}"}
;; <=

;; @@

;; @@

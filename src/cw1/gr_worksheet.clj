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
  (:require [kixi.stats.random :as ran]
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
  (ran/draw (ran/normal {:mu 0, :sd 1})))

(def w (sample_norm))
(def x (vec (repeatedly n_of_samps #(sample_norm))))
(def n (vec (repeatedly n_of_samps #(sample_norm))))

(def y (m/+ (m/* x w) n))
(def x_y [x y])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cw1_worksheet/x_y</span>","value":"#'cw1_worksheet/x_y"}
;; <=

;; @@
(plt/histogram (apply list y) :bins 40)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"ff2c72cb-22d9-4966-9e52-2cb0dfba4751","values":[{"x":-5.808412429432227,"y":0},{"x":-5.532486315430554,"y":2.0},{"x":-5.256560201428881,"y":0.0},{"x":-4.980634087427208,"y":0.0},{"x":-4.704707973425535,"y":1.0},{"x":-4.428781859423862,"y":0.0},{"x":-4.152855745422189,"y":1.0},{"x":-3.8769296314205155,"y":2.0},{"x":-3.6010035174188424,"y":5.0},{"x":-3.3250774034171693,"y":4.0},{"x":-3.049151289415496,"y":3.0},{"x":-2.773225175413823,"y":11.0},{"x":-2.49729906141215,"y":9.0},{"x":-2.221372947410477,"y":6.0},{"x":-1.9454468334088038,"y":27.0},{"x":-1.6695207194071306,"y":19.0},{"x":-1.3935946054054575,"y":19.0},{"x":-1.1176684914037844,"y":33.0},{"x":-0.8417423774021113,"y":37.0},{"x":-0.5658162634004382,"y":36.0},{"x":-0.2898901493987651,"y":37.0},{"x":-0.013964035397092012,"y":40.0},{"x":0.2619620786045811,"y":45.0},{"x":0.5378881926062542,"y":38.0},{"x":0.8138143066079273,"y":37.0},{"x":1.0897404206096004,"y":36.0},{"x":1.3656665346112735,"y":30.0},{"x":1.6415926486129466,"y":26.0},{"x":1.9175187626146197,"y":34.0},{"x":2.193444876616293,"y":14.0},{"x":2.469370990617966,"y":10.0},{"x":2.745297104619639,"y":13.0},{"x":3.021223218621312,"y":10.0},{"x":3.2971493326229853,"y":3.0},{"x":3.5730754466246584,"y":1.0},{"x":3.8490015606263315,"y":2.0},{"x":4.124927674628005,"y":3.0},{"x":4.400853788629678,"y":3.0},{"x":4.676779902631351,"y":1.0},{"x":4.952706016633024,"y":1.0},{"x":5.228632130634697,"y":1.0},{"x":5.50455824463637,"y":0}]}],"marks":[{"type":"line","from":{"data":"ff2c72cb-22d9-4966-9e52-2cb0dfba4751"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ff2c72cb-22d9-4966-9e52-2cb0dfba4751","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ff2c72cb-22d9-4966-9e52-2cb0dfba4751","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"ff2c72cb-22d9-4966-9e52-2cb0dfba4751\", :values ({:x -5.808412429432227, :y 0} {:x -5.532486315430554, :y 2.0} {:x -5.256560201428881, :y 0.0} {:x -4.980634087427208, :y 0.0} {:x -4.704707973425535, :y 1.0} {:x -4.428781859423862, :y 0.0} {:x -4.152855745422189, :y 1.0} {:x -3.8769296314205155, :y 2.0} {:x -3.6010035174188424, :y 5.0} {:x -3.3250774034171693, :y 4.0} {:x -3.049151289415496, :y 3.0} {:x -2.773225175413823, :y 11.0} {:x -2.49729906141215, :y 9.0} {:x -2.221372947410477, :y 6.0} {:x -1.9454468334088038, :y 27.0} {:x -1.6695207194071306, :y 19.0} {:x -1.3935946054054575, :y 19.0} {:x -1.1176684914037844, :y 33.0} {:x -0.8417423774021113, :y 37.0} {:x -0.5658162634004382, :y 36.0} {:x -0.2898901493987651, :y 37.0} {:x -0.013964035397092012, :y 40.0} {:x 0.2619620786045811, :y 45.0} {:x 0.5378881926062542, :y 38.0} {:x 0.8138143066079273, :y 37.0} {:x 1.0897404206096004, :y 36.0} {:x 1.3656665346112735, :y 30.0} {:x 1.6415926486129466, :y 26.0} {:x 1.9175187626146197, :y 34.0} {:x 2.193444876616293, :y 14.0} {:x 2.469370990617966, :y 10.0} {:x 2.745297104619639, :y 13.0} {:x 3.021223218621312, :y 10.0} {:x 3.2971493326229853, :y 3.0} {:x 3.5730754466246584, :y 1.0} {:x 3.8490015606263315, :y 2.0} {:x 4.124927674628005, :y 3.0} {:x 4.400853788629678, :y 3.0} {:x 4.676779902631351, :y 1.0} {:x 4.952706016633024, :y 1.0} {:x 5.228632130634697, :y 1.0} {:x 5.50455824463637, :y 0})}], :marks [{:type \"line\", :from {:data \"ff2c72cb-22d9-4966-9e52-2cb0dfba4751\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ff2c72cb-22d9-4966-9e52-2cb0dfba4751\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ff2c72cb-22d9-4966-9e52-2cb0dfba4751\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
(solve_for_w x_train y_train)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>1.258490220791166</span>","value":"1.258490220791166"}],"value":"[1.258490220791166]"}],"value":"[[1.258490220791166]]"}
;; <=

;; @@
w
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>1.2224532155446293</span>","value":"1.2224532155446293"}
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
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:n</span>","value":":n"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}],"value":"[:n 10]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:n_train</span>","value":":n_train"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[:n_train 5]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:w</span>","value":":w"},{"type":"html","content":"<span class='clj-double'>0.6609136300096325</span>","value":"0.6609136300096325"}],"value":"[:w 0.6609136300096325]"}],"value":"{:n 10, :n_train 5, :w 0.6609136300096325}"}
;; <=

;; @@

;; @@

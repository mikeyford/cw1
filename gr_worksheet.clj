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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"5337ca1a-7d2c-4415-a982-8341b68fa05c","values":[{"x":-4.812970480994763,"y":0},{"x":-4.597020643922734,"y":1.0},{"x":-4.381070806850706,"y":2.0},{"x":-4.165120969778677,"y":0.0},{"x":-3.9491711327066485,"y":1.0},{"x":-3.7332212956346202,"y":2.0},{"x":-3.517271458562592,"y":4.0},{"x":-3.3013216214905636,"y":5.0},{"x":-3.0853717844185353,"y":2.0},{"x":-2.869421947346507,"y":7.0},{"x":-2.6534721102744787,"y":4.0},{"x":-2.4375222732024504,"y":6.0},{"x":-2.221572436130422,"y":4.0},{"x":-2.005622599058394,"y":13.0},{"x":-1.7896727619863653,"y":12.0},{"x":-1.5737229249143367,"y":10.0},{"x":-1.3577730878423082,"y":20.0},{"x":-1.1418232507702797,"y":25.0},{"x":-0.9258734136982512,"y":28.0},{"x":-0.7099235766262226,"y":30.0},{"x":-0.49397373955419416,"y":33.0},{"x":-0.2780239024821657,"y":30.0},{"x":-0.062074065410137214,"y":39.0},{"x":0.15387577166189126,"y":42.0},{"x":0.36982560873391973,"y":32.0},{"x":0.5857754458059482,"y":23.0},{"x":0.8017252828779766,"y":41.0},{"x":1.0176751199500051,"y":36.0},{"x":1.2336249570220337,"y":29.0},{"x":1.4495747940940622,"y":22.0},{"x":1.6655246311660907,"y":20.0},{"x":1.8814744682381193,"y":19.0},{"x":2.0974243053101476,"y":14.0},{"x":2.313374142382176,"y":14.0},{"x":2.529323979454204,"y":4.0},{"x":2.7452738165262325,"y":8.0},{"x":2.961223653598261,"y":4.0},{"x":3.177173490670289,"y":2.0},{"x":3.3931233277423174,"y":7.0},{"x":3.6090731648143457,"y":3.0},{"x":3.825023001886374,"y":1.0},{"x":4.040972838958402,"y":1.0},{"x":4.256922676030431,"y":0}]}],"marks":[{"type":"line","from":{"data":"5337ca1a-7d2c-4415-a982-8341b68fa05c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"5337ca1a-7d2c-4415-a982-8341b68fa05c","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"5337ca1a-7d2c-4415-a982-8341b68fa05c","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"5337ca1a-7d2c-4415-a982-8341b68fa05c\", :values ({:x -4.812970480994763, :y 0} {:x -4.597020643922734, :y 1.0} {:x -4.381070806850706, :y 2.0} {:x -4.165120969778677, :y 0.0} {:x -3.9491711327066485, :y 1.0} {:x -3.7332212956346202, :y 2.0} {:x -3.517271458562592, :y 4.0} {:x -3.3013216214905636, :y 5.0} {:x -3.0853717844185353, :y 2.0} {:x -2.869421947346507, :y 7.0} {:x -2.6534721102744787, :y 4.0} {:x -2.4375222732024504, :y 6.0} {:x -2.221572436130422, :y 4.0} {:x -2.005622599058394, :y 13.0} {:x -1.7896727619863653, :y 12.0} {:x -1.5737229249143367, :y 10.0} {:x -1.3577730878423082, :y 20.0} {:x -1.1418232507702797, :y 25.0} {:x -0.9258734136982512, :y 28.0} {:x -0.7099235766262226, :y 30.0} {:x -0.49397373955419416, :y 33.0} {:x -0.2780239024821657, :y 30.0} {:x -0.062074065410137214, :y 39.0} {:x 0.15387577166189126, :y 42.0} {:x 0.36982560873391973, :y 32.0} {:x 0.5857754458059482, :y 23.0} {:x 0.8017252828779766, :y 41.0} {:x 1.0176751199500051, :y 36.0} {:x 1.2336249570220337, :y 29.0} {:x 1.4495747940940622, :y 22.0} {:x 1.6655246311660907, :y 20.0} {:x 1.8814744682381193, :y 19.0} {:x 2.0974243053101476, :y 14.0} {:x 2.313374142382176, :y 14.0} {:x 2.529323979454204, :y 4.0} {:x 2.7452738165262325, :y 8.0} {:x 2.961223653598261, :y 4.0} {:x 3.177173490670289, :y 2.0} {:x 3.3931233277423174, :y 7.0} {:x 3.6090731648143457, :y 3.0} {:x 3.825023001886374, :y 1.0} {:x 4.040972838958402, :y 1.0} {:x 4.256922676030431, :y 0})}], :marks [{:type \"line\", :from {:data \"5337ca1a-7d2c-4415-a982-8341b68fa05c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"5337ca1a-7d2c-4415-a982-8341b68fa05c\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"5337ca1a-7d2c-4415-a982-8341b68fa05c\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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

(mmul (mmul (inverse (mmul (transpose x_train) x_train)) (transpose x_train)) y_train) 
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>1.0908499035361605</span>","value":"1.0908499035361605"}],"value":"[1.0908499035361605]"}],"value":"[[1.0908499035361605]]"}
;; <=

;; @@
w
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>1.0235556913767254</span>","value":"1.0235556913767254"}
;; <=

;; @@

;; @@

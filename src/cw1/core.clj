(ns cw1.core
  (:require [kixi.stats.random :as k]
            [clojure.core.matrix.operators :as m])
  (:use [clojure.core.matrix]))



(defn sample_norm []
  (k/draw (k/normal {:mu 0, :sd 1})))


(defn multi_sample_norm [list]
  (doseq [x list]
    (intern *ns* (symbol x) (sample_norm))
    ))


(def w (sample_norm))
(def x (vec (repeatedly 600 #(sample_norm))))
(def n (vec (repeatedly 600 #(sample_norm))))

(def y (m/+ (m/* x w) n))

(defn solve
  )
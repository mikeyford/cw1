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


(defn estimate-w [n_of_samps training_set_size]
      (let [w_actual (sample_norm)
            x (vec (repeatedly n_of_samps #(sample_norm)))
            n (vec (repeatedly n_of_samps #(sample_norm)))
            y (m/+ (mmul (transpose x) w_actual) n)

            x_train (transpose (matrix [(nth (split-at training_set_size x) 0)]))
            y_train (transpose (matrix [(nth (split-at training_set_size y) 0)]))

            x_test (transpose (matrix [(nth (split-at training_set_size x) 1)]))
            y_test (transpose (matrix [(nth (split-at training_set_size y) 1)]))


            w_estimated (mmul (mmul (inverse (mmul (transpose x_train) x_train)) (transpose x_train)) y_train)
            ;y_test_estimated ()
            ]

           {:training-error (m/- w_actual w_estimated) :test-error }))
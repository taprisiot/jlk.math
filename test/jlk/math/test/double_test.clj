(ns jlk.math.test.double
  (:use clojure.test)
  (:require [jlk.math.double :as d]))

(deftest atan2-flipped
  ;; arg order of atan2 is weird, check it
  (is (== 0 (d/atan2 #_x 1 #_y 0)))
  (is (< (d/abs (- (d/atan2 0 1) (/ d/π 2))) 0.000001)))

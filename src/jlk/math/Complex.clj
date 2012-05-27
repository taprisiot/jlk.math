(ns jlk.math.Complex
  (:gen-class
   :extends org.apache.commons.math3.complex.Complex
   :prefix "method-")
  (:use [jlk.math.complex-format :only [*default-formatter*]]))

(defn method-toString
  [this]
  (.format @*default-formatter* this))

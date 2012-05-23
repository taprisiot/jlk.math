(ns jlk.math.complex
  (:use [jlk.utility.core :only [exception]])
  (:refer-clojure :exclude [format])
  (:import [org.apache.commons.math3.complex Complex ComplexFormat ComplexField ComplexUtils]))

(defn complex
  [real imag]
  (Complex. real imag))

(defn complex-polar
  [r theta]
  (ComplexUtils/polar2Complex r theta))

(defn real
  [z]
  (.getReal z))

(defn imag
  [z]
  (.getImaginary z))

;; TODO
;; formatter like on my HP:  <real>i<imag> and <mag>φ<angle>
;; (defn formatter
;;   [imag-char number-format]
;;   (ComplexFormat. imag-char number-format))

(def ^:dynamic *complex-format* (ComplexFormat. "j"))

(defn format
  ([z]
     (format z *complex-format*))
  ([z format]
     (.format format z)))

(defn parse
  ([z]
     (parse z *complex-format*))
  ([z format]
     (if-let [rv (.parse *complex-format* z)]
       rv
       (exception "cannot parse %s using %s" z format))))

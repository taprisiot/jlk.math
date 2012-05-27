(ns jlk.math.complex
  (:refer-clojure :exclude [+ - * / mod])
  (:use [jlk.utility.core :only [exception]]
        [jlk.math.complex-format :only [*default-formatter* set-format!]]
        [jlk.math.core :only [*angle-mode* *decimal-places* parse-angle]])
  (:import [org.apache.commons.math3.complex ComplexField ComplexUtils]))

(def ZERO org.apache.commons.math3.complex.Complex/ZERO)
(def ONE org.apache.commons.math3.complex.Complex/ONE)

(defn -p2r
  [x y]
  (let [z (ComplexUtils/polar2Complex x y)]
    (jlk.math.Complex. (.getReal z) (.getImaginary z))))

(defn complex
  ([real]
     (jlk.math.Complex. real))
  ([real imag]
     (jlk.math.Complex. real imag))
  ([x y format]
     (case format
       :rect (complex x y)
       :rectangular (complex x y)
       :polar  (-p2r x (parse-angle y))
       :phasor (-p2r x (parse-angle y))

       :rad (-p2r x y)
       :radians (-p2r x y)
       :polar-radians (-p2r x y)
       :phasor-radians (-p2r x y)

       :deg (-p2r x (parse-angle y))
       :degrees (-p2r x (parse-angle y))
       :polar-degrees (-p2r x (parse-angle y))
       :phasor-degrees (-p2r x (parse-angle y))
       (exception "complex: unrecognised format %s" format))))

(defn real [z] (.getReal z))

(defn imag [z] (.getImaginary z))

(defn abs [z] (.abs z))
(def modulus abs)
(def mod abs)
(def norm abs)

(defn arg [z] (.getArgument z))
(def phase arg)

(defn polar
  ([z] (polar z @*angle-mode*))
  ([z format]
     (case format
       :degrees [(abs z) (Math/toDegrees (arg z))]
       :deg [(abs z) (Math/toDegrees (arg z))]
       :radians [(abs z) (arg z)]
       :rad [(abs z) (arg z)]
       (exception "complex: unrecognised format %s" format))))

(defn conjugate
  [z] (.conjugate z))

(defn add
  ([] ZERO)
  ([x] x)
  ([x y] (.add x y))
  ([x y & rest] (reduce add (add x y) rest)))
(def + add)

(defn subtract
  ([x] (.negate x))
  ([x y] (.subtract x y))
  ([x y & rest] (reduce subtract (subtract x y) rest)))
(def sub subtract)
(def - sub)

(defn multiply
  ([] ONE)
  ([x] x)
  ([x y] (.multiply x y))
  ([x y & rest] (reduce multiply (multiply x y) rest)))
(def mul multiply)
(def * multiply)

(defn divide
  ([x] (.reciprocal x))
  ([x y] (.divide x y))
  ([x y & rest] (reduce divide (divide x y) rest)))
(def div divide)
(def / divide)

(defn square [x] (* x x))
(defn square-root [x] (.sqrt x))
(defn cube [x] (* x x x))
(defn cube-root [x] (.pow x (double 1/3)))
(defn power [x y] (.pow x y))
(defn nth-root [x y] (seq (.nthRoot x y)))

(defn exp [x] (.exp x))
(defn ln [x] (.log x))

(defn sin [x] (.sin x))
(defn cos [x] (.cos x))
(defn tan [x] (.tan x))
(defn asin [x] (.asin x))
(defn acos [x] (.acos x))
(defn atan [x] (.atan x))

(defn sinh [x] (.sinh x))
(defn cosh [x] (.cosh x))
(defn tanh [x] (.tanh x))

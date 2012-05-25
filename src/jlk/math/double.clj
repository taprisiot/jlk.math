(ns jlk.math.double)

;; to access greek letters
;; C-x 8 RET GREEK SMALL LETTER PI RET
;; or C-u C-\ TeX RET, after which
;; \pi ⇒ π, \mu ⇒ μ, \epsilon ⇒ ε, &c. (C-h I RET for more)

;;
;; arithmetic on doubles
;;

;;
;; constants
;; 

(def e Math/E)
(def π Math/PI)
(def pi π)

(def μ0 (* 4e-7 π))
(def mu_0 μ0)

(defmulti μr
  "relative permeability

values from http://en.wikipedia.org/wiki/Permeability_(electromagnetism)"
  (fn [x] x)) ;; 
(def mu_r μr)
(defmethod μr :vacuum [_] 1.0)
(defmethod μr :air [_] 1.00000037)
(defmethod μr :Cu [_] 0.999994)
(defmethod μr :Al [_] 1.000022)
(defmethod μr :steel [_] 100)

;; speed of light in a vacuum http://en.wikipedia.org/wiki/Speed_of_light
(def c0 299792458)

(def ε0 (/ 1 (* c0 c0 μ0)))
(def e_0 ε0)

(defmulti εr
  "relative permittivity

values from http://en.wikipedia.org/wiki/Relative_permittivity"
  (fn [x] x))
(def e_r εr)

(defmethod εr :vacuum [_] 1.0)
(defmethod εr :air [_] 1.00058986)


;; basic

(defn abs [x] (Math/abs x))

(defn square [x] (* x x))
(def sq square)
(defn square-root [x] (Math/sqrt x))
(def sqrt square-root)
(defn cube [x] (* x x x))
(defn cube-root [x] (Math/cbrt x))

(defn pow [x y] (Math/pow x y))
(defn nroot [x y] (pow y (/ 1 x)))
(def nrt nroot)

(defn arithmetic-mean
  [& vals]
  (/ (apply + vals) (count vals)))

(def avg arithmetic-mean)
(def average arithmetic-mean)

(defn geometric-mean
  [& vals]
  (pow (apply * vals) (/ 1 (count vals))))

(defn harmonic-mean
  "n / (1/x0 + 1/x1 + 1/x2 + ...)"
  [& vals]
  (/ (count vals) (apply + (map #(pow % -1) vals))))

;; logarithms

(defn ln [x] (Math/log x))

(defn log [x] (Math/log10 x))

;; angles

(defn degrees-to-radians [x] (Math/toRadians x))
(def deg2rad degrees-to-radians)
(def d2r degrees-to-radians)
(def to-radians degrees-to-radians)
(defn radians-to-degrees [x] (Math/toDegrees x))
(def rad2deg radians-to-degrees)
(def r2d radians-to-degrees)
(def to-degrees radians-to-degrees)

;; trig

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))
(defn asin [x] (Math/asin x))
(defn acos [x] (Math/acos x))
(defn atan [x] (Math/atan x))
(defn atan2 [x y]
  "returns theta in the conversion from (x,y) to (r,theta)"
  (Math/atan2 y x)) ;; check this

(defn sinh [x] (Math/sinh x))
(defn cosh [x] (Math/cosh x))
(defn tanh [x] (Math/tanh x))

;; misc

(defn round [x] (Math/round x))
(defn floor [x] (Math/floor x))
(defn ceiling [x] (Math/ceil x))

(defn random [] "0.0 <= x < 1.0" (Math/random))

(defn hypotenuse [x y] (Math/hypot x y))
(def hypot hypotenuse)

(defn rectangular2polar [x y]
  [(hypotenuse x y) (atan2 x y)])
(def r2p rectangular2polar)

(defn polar2rectangular [r theta]
  [(* r (cos theta)) (* r (sin theta))])
(def p2r polar2rectangular)

(ns jlk.math.double
  (:use [jlk.utility.core :only [exception]]))

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

(def small 1e-12)
(def ε small)

(def e Math/E)
(def π Math/PI)
(def pi π)

(def μ0 (* 4e-7 π))
(def mu_0 μ0)

(defmulti μr
  "relative permeability

values from http://en.wikipedia.org/wiki/Permeability_(electromagnetism)"
  identity) ;; 
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
  identity)
(def e_r εr)

(defmethod εr :vacuum [_] 1.0)
(defmethod εr :air [_] 1.00058986)

(defmacro ^:private defmalias
  "Strengthen a one-argument function-like thing to an alias bound to
a real function."
  [name mac]
  `(defn ~name
     ~(str "As with " mac ".")
     {:arglists '([~'x]) ::defmalias 1}
     [^Double x#] (~mac x#)))

;; basic

(defmalias abs Math/abs)

(defn square [x] (* x x))
(def sq square)
(defmalias square-root Math/sqrt)
(def sqrt square-root)
(defn cube [x] (* x x x))
(defmalias cube-root Math/cbrt)

(defn power [x y] (Math/pow x y))
(def pow power)
(defn nth-root [x y] (pow y (/ 1 x)))
(def nroot nth-root)
(def nrt nth-root)

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

(defmalias ln Math/log)

(defmalias log Math/log10)

;; angles

(defmalias degrees-to-radians Math/toRadians)
(def deg2rad degrees-to-radians)
(def d2r degrees-to-radians)
(def to-radians degrees-to-radians)
(defmalias radians-to-degrees Math/toDegrees)
(def rad2deg radians-to-degrees)
(def r2d radians-to-degrees)
(def to-degrees radians-to-degrees)

;; trig

(defmalias sin Math/sin)
(defmalias cos Math/cos)
(defmalias tan Math/tan)
(defmalias asin Math/asin)
(defmalias acos Math/acos)
(defmalias atan Math/atan)
(defn atan2 [x y]
  "returns theta in the conversion from (x,y) to (r,theta)"
  (Math/atan2 y x)) ;; check this

(defmalias sinh Math/sinh)
(defmalias cosh Math/cosh)
(defmalias tanh Math/tanh)

;; misc

(defmalias round Math/round)
(defmalias floor Math/floor)
(defmalias ceiling Math/ceil)

(defn random [] "0.0 <= x < 1.0" (Math/random))

(defn hypotenuse [x y] (Math/hypot x y))
(def hypot hypotenuse)

(defn rectangular2polar [x y]
  [(hypotenuse x y) (atan2 x y)])
(def r2p rectangular2polar)

(defn polar2rectangular [r theta]
  [(* r (cos theta)) (* r (sin theta))])
(def p2r polar2rectangular)

(def ^:dynamic *default-relative-error* (atom 1e-12))
(def ^:dynamic *default-absolute-error* (atom nil))

;;
;; TODO: write some test cases for this
;;
(defn approximately-equal?
  "approximately equal?

use *default-relative-error* and *default-absolute-error* if none specified.  can set either to nil"
  ([x y]
     (approximately-equal? x y
                           :rel-err @*default-relative-error*
                           :abs-err @*default-absolute-error*))
  ([x y & {:keys [rel-err abs-err]}]
     (let [rv (if rel-err (abs (/ (- x y) x)))
           av (if abs-err (abs (- x y)))]
       (if (and rv av)
         (and (< rv rel-err)
              (< av abs-err))
         (if rv
           (< rv rel-err)
         (if av
           (< av abs-err)
           (exception "must specify rel-err, abs-err or both")))))))

(def ≅ approximately-equal?)

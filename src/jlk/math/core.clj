(ns jlk.math.core
  (:use [jlk.utility.core :only [exception]]
        [jlk.math.double]
        [jlk.math.complex :exclude [format]]
        [jlk.math.optimization])
  (:use [clojure.set :only [difference]]))

(defn between
  "x0 <= x <= x1 (http://mathworld.wolfram.com/Between.html)"
  [x x0 x1]
  (and (>= x x0)
       (<= x x1)))

(defn -cat
  [{:keys [E A alpha t2 t1 W1 W2 g L T1 T2]}]
  (- (+ (* E A alpha (- t2 t1)) (/ (* (sq W1) (sq g) (sq L) E A) (* 24.0 (sq T1))) T2) T1 (/ (* (sq W2) (sq g) (sq L) E A) (* 24.0 (sq T2)))))

(defn cat
  [m]
  (let [v (difference #{:E :A :alpha :t2 :t1 :W1 :W2 :g :L :T1 :T2}
                      (keys m))]
    (if-not (= (count v) 1)
      (exception "incorrect number of variables"))
    (fn [x] (-cat (assoc m (first v) x)))))

(defn solve-test
  []
  (solve (cat {:E 65000 :A 209.3 :alpha 0.000023 :t1 15 :t2 5 :W1 0.576 :W2 0.576 :g 9.81 :L 45 :T1 1595})
         :start 1
         :min 0
         :max 100000
         :max-eval 1000))

;; (solve solvefor :start 0 min 0 :max 100000 :max-eval 100000
;;
;; the above works, but i'm not sure how to generalise it....
;;










;;   (:use [jlk.repl.core]
;; ;;        [clojure.repl]
;;         )
;;   (:import [org.apache.commons.math.linear Array2DRowFieldMatrix BlockFieldMatrix SparseFieldMatrix Array2DRowRealMatrix]
;;            [org.apache.commons.math.complex ComplexField Complex ComplexFormat]
;;            [org.apache.commons.math.util BigRealField]))

;; (defn deg2rad
;;   [x]
;;   (/ (* x Math/PI) 180.0))

;; (defn rad2deg
;;   [x]
;;   (/ (* x 180.0) Math/PI))

;; (defn real
;;   [z]
;;   (.getReal z))

;; (defn imag
;;   [z]
;;   (.getImaginary z))

;; (def ^:dynamic *complex-format* (ComplexFormat. "j"))

;; (defn complex
;;   [real imag]
;;   (proxy [Complex] [real imag]
;;     (toString [] (.format *complex-format* this))))

;; (defn polar
;;   "return complex number using radians"
;;   [mag ang]
;;   (complex (* mag (Math/cos ang)) (* mag (Math/sin ang))))

;; (defn p
;;   "return complex number using degrees"
;;   [mag ang]
;;   (polar mag (deg2rad ang)))

;; (defmulti -matrix (fn [impl type & args] [impl type]))
;; (defmethod -matrix [:array2drow :real]
;;   [_ _ v]
;;   (Array2DRowRealMatrix. v))
;; (defmethod -matrix [:array2drow :complex]
;;   [_ _ v]
;;   (Array2DRowFieldMatrix. (ComplexField/getInstance) v))
;; (defmethod -matrix [:array2drow :bigreal]
;;   [_ _ v]
;;   (Array2DRowFieldMatrix. (BigRealField/getInstance) v))

;; (defn matrix
;;   [v & {:keys [impl] :or {impl :array2drow}}]
;;   (let [type (get {Double :real
;;                    Long :real
;;                    Complex :complex
;;                    } (class (first (first v))) :type-not-supported)]
;;     (-matrix impl type (into-array (map (if :real
;;                                           double-array
;;                                           #(into-array Complex)) v)))))

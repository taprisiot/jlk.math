(ns jlk.math.core
  (:use [jlk.utility.core :only [exception]]))

(defn between
  "x0 <= x <= x1 (http://mathworld.wolfram.com/Between.html)"
  [x x0 x1]
  (and (>= x x0)
       (<= x x1)))

(defonce ^:dynamic *angle-mode* (atom :degrees))

(defn parse-angle
  "ensure user input in format *angle-mode* is in the correct computer angle format (radians)"
  [x]
  (case @*angle-mode*
    :degrees (Math/toRadians x)
    :radians x
    (exception "complex: unrecognised *angle-mode*")))

(defmulti set-angle-mode! (fn [x] x))
(defmethod set-angle-mode! :deg [_] (reset! *angle-mode* :degrees))
(defmethod set-angle-mode! :degrees [_] (reset! *angle-mode* :degrees))
(defmethod set-angle-mode! :rad [_] (reset! *angle-mode* :radians))
(defmethod set-angle-mode! :radians [_] (reset! *angle-mode* :radians))
(defmethod set-angle-mode! :default [_] (exception "*angle-mode* not known"))

(defonce ^:dynamic *decimal-places* (atom 4))
(defn set-decimal-places!
  [dp] (reset! *decimal-places* dp) nil)

;; (defn -cat
;;   [{:keys [E A alpha t2 t1 W1 W2 g L T1 T2]}]
;;   (- (+ (* E A alpha (- t2 t1)) (/ (* (sq W1) (sq g) (sq L) E A) (* 24.0 (sq T1))) T2) T1 (/ (* (sq W2) (sq g) (sq L) E A) (* 24.0 (sq T2)))))

;; (defn cat
;;   [m]
;;   (let [v (difference #{:E :A :alpha :t2 :t1 :W1 :W2 :g :L :T1 :T2}
;;                       (keys m))]
;;     (if-not (= (count v) 1)
;;       (exception "incorrect number of variables"))
;;     (fn [x] (-cat (assoc m (first v) x)))))

;; (defn solve-test
;;   []
;;   (solve (cat {:E 65000 :A 209.3 :alpha 0.000023 :t1 15 :t2 5 :W1 0.576 :W2 0.576 :g 9.81 :L 45 :T1 1595})
;;          :start 1
;;          :min 0
;;          :max 100000
;;          :max-eval 1000))

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

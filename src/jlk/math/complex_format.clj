(ns jlk.math.complex-format
  (:refer-clojure :exclude [format])
  (:import [org.apache.commons.math3.complex ComplexFormat])
  (:use [jlk.math.core :only [*decimal-places*]]
        [jlk.utility.core :only [exception]]))

(defonce ^:dynamic *default-formatter* (atom (ComplexFormat. "j")))
(defmulti set-format! (fn [x & _] (if (keyword? x) x)))
(defmethod set-format! :default [formatter] (reset! *default-formatter* formatter) nil)

;; TODO
;; improve these formatters and implement ComplexFormat properly

(defn -formatter-set-dp
  [f dp]
  (doto f
    (-> .getRealFormat (.setMinimumFractionDigits dp))
    (-> .getRealFormat (.setMaximumFractionDigits dp))
    (-> .getImaginaryFormat (.setMinimumFractionDigits dp))
    (-> .getImaginaryFormat (.setMaximumFractionDigits dp))))

(defn apache-formatter
  ([]
     (apache-formatter "j"))
  ([symbol]
     (ComplexFormat. symbol))
  ([symbol dp]
     (-formatter-set-dp (apache-formatter symbol) dp)))

(defn hp-formatter
  ([]
     (hp-formatter "i" @*decimal-places*))
  ([symbol]
     (proxy [ComplexFormat] []
       (format [^org.apache.commons.math3.complex.Complex x]
         (clojure.core/format "%s%s%s"
                              (.format (.getRealFormat this) (.getReal x))
                              symbol
                              (.format (.getImaginaryFormat this) (.getImaginary x))))))
  ([symbol dp]
     (-formatter-set-dp (hp-formatter symbol) dp)))
(defmethod set-format! :hp [_] (set-format! (hp-formatter)))

(defn polar-formatter
  ([]
     (polar-formatter "∠ " @*decimal-places*))
  ([symbol]
     (proxy [ComplexFormat] []
       (format [^org.apache.commons.math3.complex.Complex x]
         (clojure.core/format "%s%s%s"
                                (.format (.getRealFormat this) (.abs x))
                                symbol
                                (.format (.getImaginaryFormat this) (.getArgument x))))))
  ([symbol dp]
     (-formatter-set-dp (polar-formatter symbol) dp)))
(defmethod set-format! :polar [_] (set-format! (polar-formatter)))

(defn fortran-formatter
  ([]
     (fortran-formatter @*decimal-places*))
  ([dp]
     (-formatter-set-dp
      (proxy [ComplexFormat] []
        (format [^org.apache.commons.math3.complex.Complex x]
          (clojure.core/format "(%s,%s)"
                               (.format (.getRealFormat this) (.getReal x))
                               (.format (.getImaginaryFormat this) (.getImaginary x)))))
      dp)))
(defmethod set-format! :fortran [_] (set-format! (fortran-formatter)))

(defn format
  ([z]
     (format z @*default-formatter*))
  ([z format]
     (.format format z)))

(defn parse
  ([z]
     (parse z @*default-formatter*))
  ([z format]
     (if-let [rv (.parse @*default-formatter* z)]
       rv
       (exception "cannot parse %s using %s" z format))))

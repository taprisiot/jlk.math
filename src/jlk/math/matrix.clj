(ns jlk.jpower.matrix
  (:use [clojure.repl :only [doc]])
  (:import [org.apache.commons.math3.linear BlockFieldMatrix FieldLUDecomposition]))

(defn matrix
  [vals]
  (BlockFieldMatrix. (into-array (map #(into-array %) vals))))

(defn invert
  [m]
  (-> (FieldLUDecomposition. m) .getSolver .getInverse))

(defn multiply
  [m1 m2]
  (.multiply m1 m2))

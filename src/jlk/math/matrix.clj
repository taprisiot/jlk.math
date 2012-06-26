(ns jlk.math.matrix
  (:import [org.apache.commons.math3.linear BlockRealMatrix BlockFieldMatrix ArrayRealVector LUDecomposition FieldLUDecomposition CholeskyDecomposition EigenDecomposition QRDecomposition SingularValueDecomposition DefaultRealMatrixChangingVisitor]))

(defmulti matrix? class)
(defmethod matrix? BlockRealMatrix [_] true)
(defmethod matrix? BlockFieldMatrix [_] true)
(defmethod matrix? :default [_] false)

(defmulti Vector? "note capital to avoid namespace clash with clojure.core" class)
(defmethod Vector? ArrayRealVector [_] true)
(defmethod Vector? :default [_] false)

(defn classify
  [x]
  (if (number? x) :number
      (if (matrix? x) :matrix
          (if (Vector? x) :vector))))

(defn matrix
  [vals]
  (BlockRealMatrix. (into-array (map #(double-array %) vals))))

(defn fieldmatrix
  [vals]
  (BlockFieldMatrix. (into-array (map #(into-array %) vals))))

(defn Vector
  "note capital to avoid namespace clash with clojure.core"
  [& vals]
  (ArrayRealVector. (double-array vals)))

(defn transpose
  [m]
  (.transpose m))

(defn dimension
  [m]
  [(.getColumnDimension m) (.getRowDimension m)])

(defmulti decomposition (fn [_ type] type))
(defmethod decomposition :lu [m _] (LUDecomposition. m))
(defmethod decomposition :cholesky [m _] (CholeskyDecomposition. m))
(defmethod decomposition :eigen [m _] (EigenDecomposition. m 0.0)) ;; 0.0 is a dummy parameter - see apache docs
(defmethod decomposition :qr [m _] (QRDecomposition. m))
(defmethod decomposition :singular [m _] (SingularValueDecomposition. m))

(defmulti invert class)
(defmethod invert BlockRealMatrix
  [m]
  (-> (LUDecomposition. m) .getSolver .getInverse))
(defmethod invert BlockFieldMatrix
  [m]
  (-> (FieldLUDecomposition. m) .getSolver .getInverse))

(defmulti determinant class)
(defmethod determinant BlockRealMatrix
  [m]
  (-> (LUDecomposition. m) .getDeterminant))
(defmethod determinant BlockFieldMatrix
  [m]
  (-> (FieldLUDecomposition. m) .getDeterminant))

(defmulti solve
  "solve linear equation A x X = B.  By default this is the exact solution (:lu).  An additional argument can be provided to use a different decomposition that can provide a least squares solution, :cholesky, :eigen, :qr, :singular"
  (fn [& args] (vec (map class args))))
(defmethod solve [BlockRealMatrix ArrayRealVector]
  [A b]
  (-> (LUDecomposition. A) .getSolver (.solve b)))
(defmethod solve [BlockRealMatrix ArrayRealVector clojure.lang.Keyword]
  [A b type]
  (-> (decomposition A type) .getSolver (.solve b)))
;; solve for BlockFieldMatrix

(defn norm
  [m]
  (.getNorm m))

(defn frobenius
  [m]
  (.getFrobeniusNorm m))

(defmulti add (fn [& args] (vec (map classify args))))
(defmethod add [:matrix :matrix]
  [m1 m2]
  (.add m1 m2))

(defmethod add [:number :matrix]
  [s m]
  (.scalarAdd m (double s)))

(defmethod add [:matrix :number]
  [m s]
  (.scalarAdd m (double s)))

(defn subtract
  [m1 m2]
  (.sub m1 m2))

(defmulti multiply (fn [& args] (vec (map classify args))))

(defmethod multiply [:matrix :matrix]
  [m1 m2]
  (.multiply m1 m2))

(defmethod multiply [:matrix :number]
  [m s]
  (.scalarMultiply m s))

(defmethod multiply [:number :matrix]
  [s m]
  (.scalarMultiply m s))

(defmethod multiply [:vector :matrix]
  [v m]
  (.preMultiply m v))

(defmethod multiply [:matrix :vector]
  [m v]
  (.operate m v))

(defn square?
  [m]
  (.isSquare m))

(defn power
  [m n]
  (.power m n))

(defn matrix-visitor
  [visitfn]
  (proxy [DefaultRealMatrixChangingVisitor] []
    (visit [row column value] (visitfn row column value))))

(defn walk-optimized!
  [m visitor]
  (.walkInOptimizedOrder m visitor)
  m)

(defn walk-column!
  [m visitor]
  (.walkInColumnOrder m visitor)
  m)

(defn walk-row!
  [m visitor]
  (.walkInRowOrder m visitor)
  m)

(defn apply-fn
  "simplified version"
  [f m]
  (walk-optimized! (.copy m) (matrix-visitor (fn [_ _ v] (f v)))))

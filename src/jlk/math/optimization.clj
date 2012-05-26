(ns jlk.math.optimization
  (:use [jlk.utility.core :only [exception]]
        [clojure.set :only (difference)]
        [clojure.walk :only [prewalk-replace walk]])
  (:import [org.apache.commons.math3.analysis UnivariateFunction MultivariateFunction]
           [org.apache.commons.math3.analysis.solvers BisectionSolver BracketingNthOrderBrentSolver BrentSolver IllinoisSolver LaguerreSolver MullerSolver MullerSolver2 NewtonSolver PegasusSolver RegulaFalsiSolver RiddersSolver SecantSolver]
           [org.apache.commons.math3.optimization GoalType]
           [org.apache.commons.math3.optimization.general GaussNewtonOptimizer]
           [org.apache.commons.math3.optimization.direct SimplexOptimizer MultiDirectionalSimplex NelderMeadSimplex PowellOptimizer]))

(defn univariate-function
  "f is a function of a single variable (double) returning a double"
  [f]
  (reify UnivariateFunction
    (value [_ x] (f x))))

(defn solve
  [f & {:keys [max-eval solver start min max rel abs]
        :or {max-eval 1000
             solver :brent
             start 0.0
             min -1.0
             max 1.0
             rel 1e-14
             abs 1e-6}}]
  (let [max-eval max-eval
        s (case solver
            :bisection (BisectionSolver. rel abs)
            :bracketing-brent (BracketingNthOrderBrentSolver. rel abs)
            :brent (BrentSolver. rel abs)
            :illinois (IllinoisSolver. rel abs)
            ;; :laguerre (LaguerreSolver. rel abs) ;; require polynomial function
            :muller (MullerSolver. rel abs)
            :muller2 (MullerSolver2. rel abs)
            ;; :newton (NewtonSolver. abs) ;; requires differentiable function
            :pegasus (PegasusSolver. rel abs)
            :regula (RegulaFalsiSolver. rel abs)
            :ridders (RiddersSolver. rel abs)
            :secant (SecantSolver. rel abs)
            (exception "invalid solver type specified %s" solver))
        uni-fn (univariate-function f)]
    (.solve s
            max-eval
            uni-fn
            min
            max
            start)))

(defn multivariate-function
  "f is a function accepting a vector (doubles) and returning a double.  eg.  (fn [[x y z]] ...)"
  [f]
  (reify MultivariateFunction
    (value [_ x] (f x))))

(defn optimize
  "multivariate optimization

f clojure funcion - see multivariate-function
start - vector of search starting points
:max-eval
:goal
:optimizer :multi-directional, :nelder-mead, :powell
:rel - relative threshold
:abs - relative threshold

will return a map of {:value, :point}"
  [f start & {:keys [max-eval goal optimizer rel abs]
              :or {max-eval 1000
                   goal :min
                   optimizer :multi-directional
                   rel 1e-14
                   abs 1e-6}}]
;;  (println max-eval goal optimizer rel abs)
  (let [dimension (count start)
        max-eval max-eval
        o (case optimizer
            :multi-directional (doto (SimplexOptimizer. rel abs)
                                 (.setSimplex (MultiDirectionalSimplex. dimension)))
            :nelder-mead (doto (SimplexOptimizer. rel abs)
                           (.setSimplex (NelderMeadSimplex. dimension)))
            :powell (PowellOptimizer. rel abs)
            (exception "invalid optimizer specified %s" optimizer))
        multi-fn (multivariate-function f)
        goal (case goal
               :min (GoalType/valueOf "MINIMIZE")
               :max (GoalType/valueOf "MAXIMIZE")
               (exception "invalid goal type specified %s" goal))
        start (double-array start)]
;; TODO - investigate convergence checkers
;;     (.setConvergenceChecker o (SimpleScalarValueChecker. 0.01 0.01))
;;     (.setConvergenceChecker o (SimpleRealPointChecker. 0.00001 0.00001))
    (let [v (.optimize o
                       max-eval
                       multi-fn
                       goal
                       start)]
      {:value (.getValue v) :point (vec (.getPoint v))})))

(defn keywordize-args-1
  "Lift a var holding a function of purely positional, symbolic
arguments to a function of one argument, a map of all but one of those
arguments so-keyed, and the final argument."
  ([v]
     {:pre [(var? v)
            (-> v meta :arglists count (= 1))]}
     (keywordize-args-1 v (->> v meta :arglists first (map keyword))))
  ([f arg-keys]
     (let [aks (set arg-keys)]
       (fn [m]
         (let [[free & err :as free?]
               (seq (difference (set arg-keys) (keys m)))]
           (assert (and (seq free?) (empty? err)))
           (fn [x] (apply f (map (assoc m free x) arg-keys))))))))

(defn map-keywords-to-syms
  "take a map with keyword or string keys and turn into symbol keys"
  [m]
  (reduce (fn [m [k v] ] (assoc m (symbol (name k)) v)) {} m))

(defmulti -resolve-expr
  "take an unresolved count, an expression and a resolved symbol table and return a function of airity the number of unresolved variables with all other symbols replaced with the values in rvals"
  (fn [x & args] x))
(defmethod -resolve-expr 0
  [_ _ _]
  (exception "expression is fully defined, nothing to solve for"))
(defmethod -resolve-expr 1
  [_ expr rvals]
  (let [lut (reduce (fn [m [k v]] (assoc m k (if v v 'x))) {} rvals)
        rexpr (prewalk-replace lut expr)]
    (fn [x] (eval (prewalk-replace {'x x} rexpr)))))
(defmethod -resolve-expr :default
  [_ expr rvals]
  (exception "multiple unresolved variables is not yet implemented"))

;;
;; do a special case for 1 arg -> solve
;; the general case -> optimize and can be provided with a vector
;;
(defn resolve-expr
  [expr values]
  (let [values (map-keywords-to-syms values)
        ;; get a list of all the symbols in the expression and resolve them if possible
        exprvals (reduce (fn [m var] (assoc m var (ns-resolve *ns* var)))
                         {}
                         (filter symbol? (distinct (flatten (tree-seq seq? identity expr)))))
        ;; map the resolved values against symbol names
        rvals (merge exprvals values)
        ;; work out how many symbols are still unresolved
        unresolved-count (count (filter nil? (vals rvals)))]
    (println rvals)
    (println unresolved-count)
    (-resolve-expr unresolved-count expr rvals)))

;; examples:
;; (solve (resolve-expr '(- (* x x) 1) {}) :start 0 :min -10 :max 10)
;; (solve (resolve-expr '(- (* x x) y) {:y 2}) :start 0 :min -10 :max 10)
;; --> -1.141
;; (solve (resolve-expr '(- (* x x) y) {:y 2}) :start 0 :min -1 :max 10)
;; --> 1.131

(ns jlk.math.parse
  (:use [jlk.log.core :only [fatal error warn info debug trace enable-subsystem! set-level! set-logger!]]
        [jlk.log.loggers :only [detailed-console-logger]]
        [jlk.utility.core :only [exception]]
        [clojure.data.finger-tree :only [double-list]])
  (:require [jlk.log.loggers :as loggers]))

;; configure logging
(enable-subsystem!)
(set-level! :debug)
(set-logger! (detailed-console-logger))
(use 'clojure.pprint)

(defn token
  [^String re rank name sym]
  {:re re
   :rank rank
   :name name
   :sym sym})

(def ^:dynamic *tokens*
  {")" {:name ")" :rank 1 :resolve (symbol ")")}
   "(" {:name "(" :rank 1 :resolve (symbol "(")}
   "+" {:name "+" :rank 3 :resolve 'clojure.core/+}
   "-" {:name "-" :rank 3 :resolve 'clojure.core/-}})

(def ^:dynamic *variables*
  {:a 2})

(defn tokenize
  "string"
  [^String s]
  (map (fn [s] (if (= s "(")
                 (symbol "(")
                 (if (= s ")")
                   (symbol ")")
                   (read-string s))))
       (re-seq #"-?[0-9]+(?:\.[0-9]+)?|:\S+|[+-\\*/()]|\S+" s))) ;; \S doesn't pick up '+'??

(def ^:dynamic *number-format* (java.text.DecimalFormat.))

;; (defn -convert
;;   [s]
;;   (try
;;     (.parse *number-format* s)
;;     (catch Exception e (or (get *tokens* s)
;;                            (get *variables* (keyword s))))))

;; (defn -convert
;;   [s]
;;   (try
;;     (.parse *number-format* s)
;;     (catch Exception e (or (get *tokens* s)
;;                            (symbol s))))) ;; look up tokens or leave them as symbols - manage variable lookup elsewhere. (keywords might be better?)

(def ^:dynamic *ops*
  {(symbol "(") {:level 0} ;; brackets don't work...
   (symbol ")") {:level 1}
   '+ {:level 2 :args 2}
   '- {:level 2 :args 2}
   '* {:level 3 :args 2}
   '/ {:level 3 :args 2}
;   nil {:level 0 :args 0}
   })

(defn op>=
  [x y]
  (>= (get-in *ops* [x :level]) (get-in *ops* [y :level])))

(defn nargs
  [op]
  (get-in *ops* [op :args]))

(defn -fill
  [op ops args expr]
  (let [cargs (first args)
        args (rest args)]
    {:ops ops
     :args args
     :expr (list op expr cargs)}))

;; (defn fill
;;   [op ops args expr]
;;   (println "!!!")
;;   (if (op> (first ops) op)
;;     (-fill op ops args expr)
;;     {:ops (conj ops op)
;;      :args args
;;      :expr expr}))

;; fix this to append at the correct end of a finger tree?
;; https://github.com/clojure/data.finger-tree
;; http://en.wikipedia.org/wiki/Shunting-yard_algorithm
(defn apply-op
  [op ops args expr]
  (println "!!!" op ops args expr)
  (loop [op op
         ops ops
         args args
         expr expr]
    (let [[sop & _] ops]
      (println ">>>>" sop op ops args expr)
      (if (and op (op>= sop op))
        (recur op (rest ops) (rest args) (list sop expr (first args)))
        {:ops (conj ops op) :args args :expr expr}))))

(defn -shunt
  [tok ops args expr]
  ;(debug "shunting: ops: %s args: %s sym: %s expr: %s" ops args sym  expr)
  (println "-----------")
  (pprint tok)
  (pprint ops)
  (pprint args)
  (pprint expr)
  (println "-----------")
  (if (number? tok)
    {:ops ops
     :args (conj args tok)
     :expr expr}
    (if (keyword? tok)
      {:ops ops
       :args (conj args tok)
       :expr expr}
      (if (symbol? tok)
        (apply-op tok ops args expr)
        (exception "unrecognised tok %s" tok)))))

(defn apply-all-ops
  [ops args expr]
  (loop [[op & ops] (reverse (conj ops expr))
         args args
         expr '()]
    (if op
      (recur ops (rest args) (list op expr (first args)))
      expr)))

(defn shunt
  [ss]
  (loop [[tok & ss] ss
         ops '()
         args []
         expr '()]
    (if tok
      (let [{:keys [ops args expr]} (-shunt tok ops args expr)]
        (recur ss ops args expr))
      (apply-all-ops ops args expr))))

;; https://gist.github.com/1175640
(defn shunt2
  [ss]
  (loop [ss ss
         opstack '()
         expr []]
    (debug "shunt2-loop %s %s %s" (seq ss) (seq opstack) expr)
    (if (empty? ss)
      (concat expr opstack)
      (let [tok (first ss)]
        (if (number? tok)
          (recur (rest ss) opstack (conj expr tok))
          (if (keyword? tok)
            (recur (rest ss) opstack (conj expr tok))
           (if (and (symbol? tok)
                    (= tok (symbol ")")))
             (let [[popped kept] (split-with #(not= % (symbol "(")) opstack)]
               (recur (rest ss) kept (into expr popped)))
             (if (and (symbol? tok)
                      (= tok (symbol "(")))
               (let [[popped kept] (split-with #(op>= % tok) (rest opstack))]
                 (recur (rest ss) kept (into expr popped)))
               (if (symbol? tok)
                 (let [[popped kept] (split-with #(op>= % tok) opstack)]
                   (recur (rest ss) (conj kept tok) (into expr popped)))
                 (exception "could not parse"))))))))))

(defn parse-infix-to-rpn
  [s]
  (shunt2 (tokenize s)))

(defn rpn-to-sexp
  [rpnexpr]
  (loop [[v & expr] rpnexpr
         stack '()]
    (debug "rpn-to-sexp-loop %s %s %s" v expr stack)
    (if (nil? v)
      (first stack)
      (if (symbol? v)
        (let [[y x & stack] stack]
          (recur expr (conj stack (list v x y))))
        (recur expr (conj stack v))))))

(defn parse-infix
  [s]
  (rpn-to-sexp (parse-infix-to-rpn s)))

(defn parse-rpn
  [s]
  (rpn-to-sexp (tokenize s)))

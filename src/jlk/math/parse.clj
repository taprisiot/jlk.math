(ns jlk.math.parse
  (:use [jlk.log.core :only [fatal error warn info debug trace enable-subsystem! disable-subsystem! set-level! set-logger!]]
        [jlk.log.loggers :only [detailed-console-logger]]
        [jlk.utility.core :only [exception enforced-split-at]]
        [clojure.data.finger-tree :only [double-list]]
        [clojure.core.match :only [match]])
  (:require [jlk.log.loggers :as loggers]))

;; configure logging
;(enable-subsystem!)
;(set-level! :debug)
;(set-logger! (detailed-console-logger))

(def <LP> (symbol "("))
(def <RP> (symbol ")"))

;; note that "sin(3)" will currently fail, but sin (3) is OK
;; this might be OK as I do not currently have a notation for functions, instead relying on binding *ops*
(defn tokenize
  "string"
  [^String s]
  (map (fn [s] (case s
                 "(" <LP>
                 ")" <RP>
                 (read-string s)))
       (re-seq #"-?[0-9]+(?:\.[0-9]+)?|:\S+|[+-\\*/()]|\S+" s))) ;; \S doesn't pick up '+'??

(def ^:dynamic *max-level* 255)
(def ^:dynamic *ops*
  (atom {'+ {:level 2 :args 2}
         '- {:level 2 :args 2}
         '* {:level 3 :args 2}
         '/ {:level 3 :args 2}
         }))

(defn op>=
  [x y]
  (let [ops @*ops*]
    (>= (get-in ops [x :level] 0) (get-in ops [y :level] *max-level*)))) ;; default value prevents null pointer, but not sure to default to a low or high value.  update: should be high

(defn nargs
  [op]
  (get-in @*ops* [op :args] 1))

(defn shunt
  "take a sequence of tokens and turn into a sequence in rpn"
  ;; note this does not adequately handle bad input
  [ss]
  (loop [ss ss
         opstack '()
         expr []
         depth 0]
    (debug "shunt-loop %s %s %s %s" (seq ss) (seq opstack) expr depth)
    (let [[token & ss] ss]
      (match [token]
        [nil] (concat expr opstack)
        [(a :when [number?])] (recur ss opstack (conj expr token) depth)
        [(b :when [keyword?])] (recur ss opstack (conj expr token) depth)
        [(c :when [symbol? #(= % <LP>)])] (recur ss (conj opstack depth) expr (inc depth))
        [(d :when [symbol? #(= % <RP>)])] (let [[popped kept]
                                                (split-with #(not= % (dec depth)) opstack)]
                                            (recur ss (rest kept) (into expr popped) (dec depth)))
        [(e :when [symbol?])] (let [[popped kept]
                                    (split-with #(op>= % token) opstack)]
                                (recur ss (conj kept token) (into expr popped) depth))
        [_] (exception "token %s not matched" token)))))

(defn parse-infix-to-rpn
  [s]
  (shunt (tokenize s)))

(defn rpn-to-sexp
  "convert rpn notation to sexp.  when evaluating functions will remove n items from the stack as defined in *ops* or 1 (the most common case) otherwise.  can define operations as :args :stack in *ops* to get the number of arguments from the top of the stack"
  [rpnexpr]
  (loop [[v & expr] rpnexpr
         stack '()]
    (debug "rpn-to-sexp-loop %s %s %s" v expr (seq stack))
    (if (nil? v)
      (if (second stack)
        (exception "invalid expression %s -> %s, %s" rpnexpr (first stack) (second stack))
        (first stack))
      (if (symbol? v)
        (let [nargs (nargs v)
              [args stack] (if (= nargs :stack)
                             (enforced-split-at (first stack) (rest stack) "invalid expression %s, \"%s\"->nargs=%s, stack=%s" rpnexpr v nargs stack)
                             (enforced-split-at nargs stack "invalid expression %s, \"%s\"->nargs=%s, stack=%s" rpnexpr v nargs stack))]
          (recur expr (conj stack (conj (reverse args) v))))
        (recur expr (conj stack v))))))

(defn parse-infix
  [s]
  (rpn-to-sexp (parse-infix-to-rpn s)))

(defn parse-rpn
  [s]
  (rpn-to-sexp (tokenize s)))


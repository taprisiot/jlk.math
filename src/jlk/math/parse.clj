(ns jlk.math.parse
  (:use [jlk.log.core :only [fatal error warn info debug trace enable-subsystem! disable-subsystem! set-level! set-logger!]]
        [jlk.log.loggers :only [detailed-console-logger]]
        [jlk.utility.core :only [exception enforced-split-at]]
        [clojure.data.finger-tree :only [double-list]])
  (:require [jlk.log.loggers :as loggers]))

;; configure logging
;(enable-subsystem!)
;(set-level! :debug)
;(set-logger! (detailed-console-logger))

(def <LP> (symbol "("))
(def <RP> (symbol ")"))

(defn tokenize
  "string"
  [^String s]
  (map (fn [s] (case s
                 "(" <LP>
                 ")" <RP>
                 (read-string s)))
       (re-seq #"-?[0-9]+(?:\.[0-9]+)?|:\S+|[+-\\*/()]|\S+" s))) ;; \S doesn't pick up '+'??

(def ^:dynamic *ops*
  (atom {<LP> {:level 0}
         <RP> {:level 1}
         '+ {:level 2 :args 2}
         '- {:level 2 :args 2}
         '* {:level 3 :args 2}
         '/ {:level 3 :args 2}
         }))

(defn op>=
  [x y]
  (let [ops @*ops*]
    (>= (get-in ops [x :level] 0) (get-in ops [y :level] 0)))) ;; default value prevents null pointer, but not sure to default to a low or high value

(defn nargs
  [op]
  (get-in @*ops* [op :args] 1))

(defn shunt
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
                    (= tok <RP>))
             (let [[popped kept] (split-with #(not= % <LP>) opstack)]
               (recur (rest ss) kept (into expr popped)))
             (if (and (symbol? tok)
                      (= tok <LP>))
               (let [[popped kept] (split-with #(op>= % tok) (rest opstack))]
                 (recur (rest ss) kept (into expr popped)))
               (if (symbol? tok)
                 (let [[popped kept] (split-with #(op>= % tok) opstack)]
                   (recur (rest ss) (conj kept tok) (into expr popped)))
                 (exception "could not parse"))))))))))

(defn parse-infix-to-rpn
  [s]
  (shunt (tokenize s)))

(defn rpn-to-sexp
  "convert rpn notation to sexp.  when evaluating functions will remove n items from the stack as defined in *ops* or 1 (the most common case) otherwise.  can define operations as :args :stack in *ops* to get the number of arguments from the top of the stack"
  [rpnexpr]
  (loop [[v & expr] rpnexpr
         stack '()]
    (debug "rpn-to-sexp-loop %s %s %s" v expr stack)
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

(ns jlk.math.test.parse
  (:use [clojure.test]
        [jlk.log.core :only [fatal error warn info debug trace enable-subsystem! disable-subsystem! set-level! set-logger!]]
        [jlk.math.parse :only [<LP> <RP> <CARET>]]
        [jlk.math.double :only [power]])
  (:require [jlk.math.parse :as parse]))

;; configure logging
;(enable-subsystem!)
;(set-level! :debug)

(defn rs [tokens rpnexpr sexpr eval result]
  {:ts tokens :rpnexpr rpnexpr :sexpr sexpr :eval? eval :result result})

(def ^:dynamic *infix-input*
  [;; simple input
   ["" {:ts '() :rpnexpr '() :sexpr nil :eval? true :result nil}]
   ["0" {:ts '(0) :rpnexpr '(0) :sexpr 0 :eval? true :result 0}]
   ["1" {:ts '(1) :rpnexpr '(1) :sexpr 1 :eval? true :result 1}]
   ["-1" {:ts '(-1) :rpnexpr '(-1) :sexpr -1  :eval? true :result -1}]
   ["0.0" {:ts '(0.0) :rpnexpr '(0.0) :sexpr 0.0 :eval? true :result 0.0}]
   ["0.1" {:ts '(0.1) :rpnexpr '(0.1) :sexpr 0.1 :eval? true :result 0.1}]
   ["-0.1" {:ts '(-0.1) :rpnexpr '(-0.1) :sexpr -0.1 :eval? true :result -0.1}]
   ;;; "1/2" 1/2 ;; ideally
   ;; basic operations
   ["0+0" {:ts '(0 + 0) :rpnexpr '(0 0 +) :sexpr '(+ 0 0) :eval? true :result 0}]
   [" 0+0" {:ts '(0 + 0) :rpnexpr '(0 0 +) :sexpr '(+ 0 0) :eval? true :result 0}]
   ["0+0 " {:ts '(0 + 0) :rpnexpr '(0 0 +) :sexpr '(+ 0 0) :eval? true :result 0}]
   [" 0+0 " {:ts '(0 + 0) :rpnexpr '(0 0 +) :sexpr '(+ 0 0) :eval? true :result 0}]
   ["0 + 0" {:ts '(0 + 0) :rpnexpr '(0 0 +) :sexpr '(+ 0 0) :eval? true :result 0}]
   ["0+ 0" {:ts '(0 + 0) :rpnexpr '(0 0 +) :sexpr '(+ 0 0) :eval? true :result 0}]
   ["0 +0" {:ts '(0 + 0) :rpnexpr '(0 0 +) :sexpr '(+ 0 0) :eval? true :result 0}]
   ["0*0" {:ts '(0 * 0) :rpnexpr '(0 0 *) :sexpr '(* 0 0) :eval? true :result 0}]
   ;; keywords
   [":a" {:ts '(:a) :rpnexpr '(:a) :sexpr :a :eval? true :result :a}]
   [":0" {:ts '(:0) :rpnexpr '(:0) :sexpr :0 :eval? true :result :0}]
   [":a + 1" {:ts '(:a + 1) :rpnexpr '(:a 1 +) :sexpr '(+ :a 1) :eval false}]
   ;; order of operations
   ["1 + 2 * 3" (rs '(1 + 2 * 3) '(1 2 3 * +) '(+ 1 (* 2 3)) true 7)]
   ["1 * 2 + 3" (rs '(1 * 2 + 3) '(1 2 * 3 +) '(+ (* 1 2) 3) true 5)]
   ["1 - 2 * 3" (rs '(1 - 2 * 3) '(1 2 3 * -) '(- 1 (* 2 3)) true -5)]
   ["1 * 2 - 3" (rs '(1 * 2 - 3) '(1 2 * 3 -) '(- (* 1 2) 3) true -1)]
   ["1 + 2 / 3" (rs '(1 + 2 / 3) '(1 2 3 / +) '(+ 1 (/ 2 3)) true 5/3)]
   ["1 / 2 + 3" (rs '(1 / 2 + 3) '(1 2 / 3 +) '(+ (/ 1 2) 3) true 7/2)]
   ;; parenthesis
   ["(0)" (rs (list <LP> '0 <RP>) '(0) 0 true 0)]
   ["(1 + 2) * 3" (rs (list <LP> 1 '+ 2 <RP> '* 3) '(1 2 + 3 *) '(* (+ 1 2) 3) true 9)]
   ["(1 * 2) + 3" (rs (list <LP> 1 '* 2 <RP> '+ 3) '(1 2 * 3 +) '(+ (* 1 2) 3) true 5)]
   ["1 + (2 * 3)" (rs (list 1 '+ <LP> 2 '* 3 <RP>) '(1 2 3 * +) '(+ 1 (* 2 3)) true 7)]
   ["1 * (2 + 3)" (rs (list 1 '* <LP> 2 '+ 3 <RP>)
                      '(1 2 3 + *)
                      '(* 1 (+ 2 3))
                      true 5)]
   ;; multiple parenthesis
   ["(1 * (2 * (3 + 4)))" (rs (list <LP> 1 '* <LP> 2 '* <LP> 3 '+ 4 <RP> <RP> <RP>)
                              '(1 2 3 4 + * *)
                              '(* 1 (* 2 (+ 3 4)))
                              true 14)]
   ;; arbitrary
   ["1 / 2 + (2) * 3 - ((3 / 2) / 2)" (rs (list 1 '/ 2 '+ <LP> 2 <RP> '* 3 '- <LP> <LP> 3 '/ 2 <RP> '/ 2 <RP>)
                                          '(1 2 / 2 3 * + 3 2 / 2 / -)
                                          '(- (+ (/ 1 2) (* 2 3)) (/ (/ 3 2) 2))
                                          true
                                          23/4)]
   ;; exponentiation
   ["1^1" (rs (list 1 <CARET> 1) '(1 1 power) '(power 1 1) true 1.0)]
   ["1^2" (rs (list 1 <CARET> 2) '(1 2 power) '(power 1 2) true 1.0)]
   ["2^1" (rs (list 2 <CARET> 1) '(2 1 power) '(power 2 1) true 2.0)]
   ["2^2" (rs (list 2 <CARET> 2) '(2 2 power) '(power 2 2) true 4.0)]
   ["1^2^3" (rs (list 1 <CARET> 2 <CARET> 3)
                '(1 2 3 power power) '(power 1 (power 2 3)) true 1.0)]
   ["3^2^1" (rs (list 3 <CARET> 2 <CARET> 1)
                '(3 2 1 power power) '(power 3 (power 2 1)) true 9.0)]
   ["2 * 3^2" (rs (list 2 '* 3 <CARET> 2)
                  '(2 3 2 power *) '(* 2 (power 3 2)) true 18.0)]
   ["3^(2 * 2)" (rs (list 3 <CARET> <LP> 2 '* 2 <RP>)
                    '(3 2 2 * power) '(power 3 (* 2 2)) true 81.0)]
   ]
  )

(deftest tokenize-infix-input
  (doseq [[input {:keys [ts]}] *infix-input*]
    (is (= ts (parse/tokenize input)))))

(deftest parse-infix-input-to-rpn
  (doseq [[input {:keys [rpnexpr]}] *infix-input*]
    (debug "testing that \"%s\" parses to (rpn) %s" input rpnexpr)
    (is (= rpnexpr (parse/parse-infix-to-rpn input)))))

(deftest parse-infix
  (doseq [[input {:keys [sexpr]}] *infix-input*]
    (debug "testing that \"%s\" parses to (sexpr) %s" input sexpr)
    (is (= sexpr (parse/parse-infix input)))))

(deftest eval-infix
  (doseq [[input {:keys [result]}] (filter #(get-in % [1 :eval?]) *infix-input*)]
    (debug "testing that \"%s\" evaluates to %s" input result)
    (is (= result (eval (parse/parse-infix input))))))

;; (deftest parse-rpn-input
;;   )

(defn sin
  "a sample function of one argument - no additional definition necessary"
  [x]
  (Math/sin x))

(defn hypot
  "a sample function of two arguments - need to define {:args 2} in the *ops* table."
  [x y]
  (Math/sqrt (+ (* x x) (* y y))))

(defn avg
  "a sample function of n arguments - need to define {:args :stack} in the *ops* table"
  [& args]
  (if (> (count args) 0)
    (/ (reduce + args) (count args))
    0))

(deftest rpn-to-sexp
  ;; test some dodgy input
  (doseq [expr ['(0 0)
                '(0 0 0 +)]]
    (debug "testing that %s throws an Exception" expr)
    (is (thrown? Exception (parse/rpn-to-sexp expr))))

  ;; test additional functions
  (let [oo @parse/*ops*]
    (swap! parse/*ops* assoc
           'hypot {:args 2}
           'avg {:args :stack}
           ;; 'sin uses the default number of arguments, 1, and does not need to be explicitly defined
           )
    (is (= (parse/rpn-to-sexp '(0 sin)) '(sin 0)))
    (is (= (eval (parse/rpn-to-sexp '(0 sin))) 0.0))
    
    (is (= (parse/rpn-to-sexp '(1 1 hypot)) '(hypot 1 1)))
    (is (= (eval (parse/rpn-to-sexp '(1 1 hypot))) (Math/sqrt 2)))

    (is (= (parse/rpn-to-sexp '(1 2 3 4 5 5 avg)) '(avg 1 2 3 4 5)))
    (is (= (eval (parse/rpn-to-sexp '(1 2 3 4 5 5 avg))) 3))
    (reset! parse/*ops* oo)))

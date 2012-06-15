(ns jlk.math.test.parse
  (:use [clojure.test]
        [jlk.log.core :only [fatal error warn info debug trace enable-subsystem! set-level! set-logger!]])
  (:require [jlk.math.parse :as parse]))

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
   ["(1 + 2) * 3" (rs '((symbol "(") 1 + 2 (symbol ")") * 3) '(1 2 + 3 *) '(* (+ 1 2) 3) true 9)]
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

;; (deftest rpn-to-sexpr)

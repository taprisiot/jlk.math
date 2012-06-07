(ns jlk.math.test.optimization
  (:use clojure.test
        jlk.math.double)
  (:require [jlk.math.optimization :as o]))

(defn vecxyz [z y x] [x y z])

(deftest test-keywordize-args
  (is (= [1 2 3] (((o/keywordize-args-1 #'vecxyz)
                   {:x 1, :z 3}) 2)))
  (is (thrown? AssertionError
               ((o/keywordize-args-1 #'vecxyz) {:x 1 :y 2 :z 3})))
  (is (thrown? AssertionError
               ((o/keywordize-args-1 #'vecxyz) {:x 1}))))

(defn cat
  "example math function"
  [E A alpha t2 t1 W1 W2 g L T1 T2]
  (- (+ (* E A alpha (- t2 t1))
        (/ (* (sq W1) (sq g) (sq L) E A)
           (* 24.0 
              (sq T1))) T2)
     T1
     (/ (* (sq W2) (sq g) (sq L) E A) (* 24.0 (sq T2)))))

(deftest solve-cat
  (is (number? (o/solve ((o/keywordize-args-1 #'cat)
                         {:E 65000 :A 209.3 :alpha 0.000023
                          :t1 15 :t2 5 :W1 0.576 :W2 0.576
                          :g 9.81 :L 45 :T1 1595})
                        :start 1
                        :min 0
                        :max 100000
                        :max-eval 1000))
      "completed run"))

(ns jlk.math.complex-generic
  (:refer-clojure :exclude [format + - * /])
  (:use [jlk.math.generic])
  (:require [jlk.math.complex :as complex])
  (:import [org.apache.commons.math3.complex Complex]))

(defmethod + [] [] (complex/add))
(defmethod + [Complex] [x] (complex/add x))
(defmethod + [Complex Complex] [x y] (complex/add x y))
(defmethod + [Complex Complex Complex] [x y z] (complex/add x y z))

(defmethod - [Complex] [x] (complex/sub x))
(defmethod - [Complex Complex] [x y] (complex/sub x y))
(defmethod - [Complex Complex Complex] [x y z] (complex/sub x y z))

(defmethod * [] [] (complex/mul))
(defmethod * [Complex] [x] (complex/mul x))
(defmethod * [Complex Complex] [x y] (complex/mul x y))
(defmethod * [Complex Complex Complex] [x y z] (complex/mul x y z))

(defmethod / [Complex] [x] (complex/div x))
(defmethod / [Complex Complex] [x y] (complex/div x y))
(defmethod / [Complex Complex Complex] [x y z] (complex/div x y z))

(defmethod square [Complex] [x] (complex/square x))
(defmethod square-root [Complex] [x] (complex/square-root x))
(defmethod cube [Complex] [x] (complex/cube x))
(defmethod cube-root [Complex] [x] (complex/cube-root x))
(defmethod power [Complex Complex] [x y] (complex/power x y))
(defmethod power [Complex Double] [x y] (complex/power x y))
(defmethod power [Complex Long] [x y] (complex/power x (double y)))
(defmethod nth-root [Complex Long] [x y] (complex/nth-root x y))
(defmethod nth-root [Complex Integer] [x y] (complex/nth-root x y))

(defmethod exp [Complex] [x] (complex/exp x))
(defmethod ln [Complex] [x] (complex/ln x))

(defmethod sin [Complex] [x] (complex/sin x))
(defmethod cos [Complex] [x] (complex/cos x))
(defmethod tan [Complex] [x] (complex/tan x))
(defmethod asin [Complex] [x] (complex/asin x))
(defmethod acos [Complex] [x] (complex/acos x))
(defmethod atan [Complex] [x] (complex/atan x))

(defmethod sinh [Complex] [x] (complex/sinh x))
(defmethod cosh [Complex] [x] (complex/cosh x))
(defmethod tanh [Complex] [x] (complex/tanh x))

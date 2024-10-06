(ns clj-stack.fixtures.layer-1
  (:require [clj-stack.fixtures.layer-2 :as layer-2]))

(defn layer-1-function-2 [arg]
  (when (> arg 10)
    (layer-2/layer-2-function-2 arg)))

(defn layer-1-function-1 [args]
  (layer-2/layer-2-function-1 args))

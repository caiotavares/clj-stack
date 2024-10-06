(ns clj-stack.fixtures.layer-2
  (:require [clj-stack.fixtures.layer-3 :as layer-3]))

(defn layer-2-function-1 [args]
  (layer-3/layer-3-function-2 args)
  (layer-3/layer-3-function-1 {:received-args args}))

(defn layer-2-function-2 [args])

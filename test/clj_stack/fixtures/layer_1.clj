(ns clj-stack.fixtures.layer-1
  (:require [clj-stack.fixtures.layer-2 :as layer-2]))

(defn L1F1 [arg]
  (when (> arg 10)
    (layer-2/L2F2 arg)))

(defn L1F2 [args]
  (layer-2/L2F1 args))

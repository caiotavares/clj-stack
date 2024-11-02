(ns flux.fixtures.layer-2
  (:require [flux.fixtures.layer-3 :as layer-3]))

(defn L2F1 [args]
  (layer-3/L3F2 args)
  (layer-3/L3F1 {:received-args args}))

(defn L2F2 [args])

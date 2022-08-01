(ns clj-stack.core-test
  (:require [clojure.test :refer :all]
            [clj-stack.core :refer [deftraced *stack*]]))

(defn layer-3-function-2 [args])

(defn layer-3-function-1 [args]
  "do nothing")

(defn layer-2-function-1 [args]
  (layer-3-function-1 {:received-args args})
  (layer-3-function-2 args))

(defn layer-1-function-2 [args]
  {:function-1 args})

(defn layer-1-function-1 [args]
  (layer-2-function-1 args))

(deftraced entrypoint [args]
  (let [banana  (layer-1-function-1 args)
        abacate (layer-1-function-2 args)
        maca    (clojure.string/capitalize "minusculo")])
  {:status 200 :body args})

(entrypoint 1)
(deref *stack*)

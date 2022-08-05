(ns clj-stack.core-test
  (:require [clojure.test :refer :all]
            [clj-stack.core :refer [deftraced]]
            [clj-stack.state :as state]))

(defn layer-3-function-2 [args])

(defn layer-3-function-1 [args]
  (throw (ex-info "OPA" {:reason "DEU RUIM"})))

(defn layer-2-function-1 [args]
  (layer-3-function-2 args)
  (layer-3-function-1 {:received-args args}))

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
state/*stack*

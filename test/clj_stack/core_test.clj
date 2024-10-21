(ns clj-stack.core-test
  (:require [clj-stack.core :refer [deftraced]]
            [clj-stack.fixtures.layer-1 :as fixtures.layer-1]
            [clj-stack.state :as state]
            [clojure.test :refer :all]))

(deftraced entrypoint [args]
  (let [abacate (fixtures.layer-1/layer-1-function-2 args)
        banana  (fixtures.layer-1/layer-1-function-1 args)
        maca    (clojure.string/capitalize "minusculo")])
  {:status 200 :body args})

(entrypoint 1)

(deref state/*stack*)

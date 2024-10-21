(ns clj-stack.core-test
  (:require [clj-stack.core :refer [deftraced]]
            [clj-stack.fixtures.layer-1 :as fixtures.layer-1]
            [clj-stack.state :as state]
            [clojure.test :refer :all]))

(deftraced entrypoint [args]
  (let [abacate (fixtures.layer-1/L1F1 args)
        banana  (fixtures.layer-1/L1F2 args)
        maca    (clojure.string/capitalize "minusculo")])
  {:status 200 :body args})

(entrypoint 1)

(deref state/*stack*)

(state/render-sequential-stack)

(:clj-stack.fixtures.layer-2/L2F1
 (:children
   (:clj-stack.fixtures.layer-1/L1F2
     (:children
       (:clj-stack.core-test/entrypoint @state/*sequential-stack*)))))

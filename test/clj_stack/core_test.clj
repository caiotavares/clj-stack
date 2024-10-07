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

(->> (map (fn [[k v]] [k (:level v)]) (deref state/*stack*))
     (filter (fn [[k v]] (= 0 v))))

(entrypoint 1)

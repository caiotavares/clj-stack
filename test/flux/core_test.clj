(ns flux.core-test
  (:require [flux.core :refer [deftraced]]
            [flux.fixtures.layer-1 :as fixtures.layer-1]
            [flux.output :as output]
            [flux.state :as state]
            [clojure.test :refer :all]))

(deftraced entrypoint [args]
  (let [abacate (fixtures.layer-1/L1F1 args)
        banana  (fixtures.layer-1/L1F2 args)
        maca    (clojure.string/capitalize "minusculo")])
  {:status 200 :body args})

(entrypoint 1)

(state/stack)
(state/sequential-stack)

(output/->file)

(state/sequential-stack)

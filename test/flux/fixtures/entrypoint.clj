(ns flux.fixtures.entrypoint
  (:require [clojure.test :refer :all]
            [flux.api :refer [deftraced]]
            [flux.fixtures.layer-1 :as fixtures.layer-1]))

(deftraced ^{:layout :flat :print? true :trace? false}
  entrypoint
  [args]
  (let [res1 (fixtures.layer-1/L1F1 args)
        res2 (fixtures.layer-1/L1F2 args)
        res3 (clojure.string/capitalize "minusculo")])
  {:status 200 :body args})

(ns clj-stack.output
  (:require [clj-stack.state :as state]
            [clj-stack.utils :as utils]))

(defn ->file [file]
  )

(defn state->d3 []
  )

(def level :clj-stack.core-test/entrypoint)

(defn make-heritage [level layer]
  (if-let [children (state/children level)]
    (doseq [{:keys [name]} children]
      (make-heritage name (update layer [:children name] conj {name {:children {}}})))
    (utils/tap layer)))

(state/sorted-stack)

(make-heritage (state/root) {level {:children {}}})

(map (fn [[k v]] (:level v)) @state/*stack*)

{:clj-stack.core-test/entrypoint :children {}}

{:clj-stack.core-test/entrypoint
 :children {:clj-stack.fixtures.layer-1/layer-1-function-1
            :children []}}

{:clj-stack.core-test/entrypoint
 :children {:clj-stack.fixtures.layer-1/layer-1-function-1
            :children {:clj-stack.fixtures.layer-2/layer-2-function-1
                       :children []}}}

{:clj-stack.core-test/entrypoint
 :children {:clj-stack.fixtures.layer-1/layer-1-function-1
            :children {:clj-stack.fixtures.layer-2/layer-2-function-1
                       :children []}}
 {:clj-stack.fixtures.layer-1/layer-1-function-2
  :children []}}

{:name     :clj-stack.core-test/entrypoint
 :children [{:name     :clj-stack.fixtures.layer-1/layer-1-function-1
             :children [{:name     :clj-stack.fixtures.layer-2/layer-2-function-1
                         :children []}]}
            {:name     :clj-stack.fixtures.layer-1/layer-1-function-2
             :children [{:name     :clj-stack.fixtures.layer-2/layer-1-function-2
                         :children []}]}]}

{:name     :clj-stack.core-test/entrypoint
 :children [{:name     :clj-stack.fixtures.layer-1/layer-1-function-1
             :children [{:name     :clj-stack.fixtures.layer-2/layer-2-function-1
                         :children []}]}
            {:name     :clj-stack.fixtures.layer-1/layer-1-function-2
             :children [{:name     :clj-stack.fixtures.layer-2/layer-1-function-2
                         :children [{:name     :clj-stack.fixtures.layer-3/layer-1-function-1
                                     :children []}]}]}]}

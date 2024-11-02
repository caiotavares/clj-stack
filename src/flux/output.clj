(ns flux.output
  (:require [flux.state :as state]
            [clojure.data.json :as json]))

(defn ->file []
  (spit "/tmp/stack.json"
        (json/write-str
          (state/sequential-stack))))

(ns clj-stack.output
  (:require [clj-stack.state :as state]
            [clojure.data.json :as json]))

(defn ->file [file]
  (spit "/tmp/stack.json"
   (json/write-str
     (state/render-sequential-stack))))


(ns flux.playground
  (:require [clojure.test :refer :all]
            [flux.api :as api]
            [flux.file :as file]
            [flux.fixtures.entrypoint :as fixtures.entrypoint]
            [flux.state :as state]))

(comment
  (fixtures.entrypoint/entrypoint 1)

  (api/get-stack :flat)

  (state/sorted-stack)
  (state/tree-stack {})

  (file/->file))


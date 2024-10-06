(ns clj-stack.tracing
  (:require [clj-stack.state :as state]
            [clj-stack.utils :as utils])
  (:import (clojure.lang ExceptionInfo)))

(defn traced!
  "Register In/Out for this fn in the call tree and prints to stdout"
  [fn-name fn args]
  (let [var-name (if (symbol? fn-name) (resolve fn-name) fn-name)
        node     (utils/namespaced var-name)]
    (state/register-input! node args)
    (try
      (let [result (apply fn args)]
        (state/register-output! node result)
        result)
      (catch ExceptionInfo e
        (state/register-exception! node e)
        (throw e)))))

(defn ^:private trace-vars* [v]
  (alter-var-root v #(fn tracing-wrapper [& args]
                       (traced! v % args))))

(defn trace-stack []
  (mapv trace-vars* (state/flat-stack)))

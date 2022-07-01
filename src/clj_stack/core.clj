(ns clj-stack.core
  (:require [clojure.pprint :as pprint]))

(defmacro defn [& fn]
  `(clojure.core/defn ~@fn))

(def *args* (atom {}))

(defn function-2 [args]
  {:status 200 :body args})

(defn function-1-1 [args]
  (let [fn-symbol (-> (Thread/currentThread)
                      .getStackTrace
                      second
                      .getClassName
                      clojure.repl/demunge
                      symbol)
        arglist (-> fn-symbol find-var meta :arglists)]
    (swap! *args* merge {(keyword fn-symbol) {:arglist       arglist
                                              :provided-args [args]}})
    (+ 1 1)))


(function-1-1 {:banana 1})

(@*args*)

;(macroexpand-1 '(with-probe))


(defn function-1 [args]
  (let [opa (function-1-1 {:args1-1 args})]
    (mapv #(print (.toString %)) (.getStackTrace (Thread/currentThread)))
    (function-2 {:args2 opa})))

(function-1 {:banana 1})
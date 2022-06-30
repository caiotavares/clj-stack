(ns clj-stack.core
  (:use [criterium.core]))

(defmacro with-probe [& forms]
  `())

(defn function-2 [args]
  {:status 200 :body args})

(defn function-1-1 [args]
  (+ 1 1))

(defn function-1 [args]
  (let [opa (function-1-1 {:args1-1 args})]
    (mapv #(clojure.pprint/pprint (.toString %)) (.getStackTrace (Thread/currentThread)))
    (function-2 {:args2 opa})))

(clojure.core/tap-loop)

(function-1 {:banana 1})

(def ^Thread thread (Thread/currentThread))


(def  stacktrace (.getStackTrace thread))
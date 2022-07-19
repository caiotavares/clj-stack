(ns clj-stack.core
  (:require [clojure.tools.trace :as trace]))

(defn analyze [s symbols]
  (cond
    (symbol? s)
    (swap! symbols conj (resolve s))

    (seqable? s)
    (doseq [s* s]
      (analyze s* symbols))))

(defmacro inspect [& fn-decl]
  (let [symbols (atom [])]
    (doseq [s fn-decl]
      (analyze s symbols))
    (prn (remove nil? @symbols)))
  `(clojure.core/defn ~@fn-decl))

(defn do-side-effect [args])

(defn function-2 [args]
  (do-side-effect args))

(macroexpand-1 '(inspect function-3 [args]
                  (let [banana (function-2 args)
                        maca (clojure.string/capitalize "minusculo")])
                  {:status 200 :body args}))

(defn function [args]
  (function-2 args))

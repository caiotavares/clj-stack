(ns clj-stack.core
  (:require [clojure.tools.trace :as trace]))

(defn matches-namespace? [ns symbol]
  (= ns
     (-> symbol meta :ns str)))

(defn clean-symbols [ns symbols]
  (->> symbols
       (remove nil?)
       (filter (partial matches-namespace? ns))))

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
    (prn (clean-symbols "clj-stack.core" @symbols)))
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

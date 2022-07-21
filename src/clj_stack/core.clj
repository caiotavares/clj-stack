(ns clj-stack.core
  (:require [clojure.tools.trace :as trace]))

(defn matches-namespace? [ns symbol]
  (some? (re-matches (re-pattern (str "[" ns "].+"))
                     (-> symbol meta :ns str))))

(defn trace-vars* [v]
  (nu/tap v)
  (trace/trace-vars v))

(defn trace-vars [ns symbols]
  (->> symbols
       (remove nil?)
       (filter (partial matches-namespace? ns))
       (mapv trace-vars*)))

(defn form->var [s symbols]
  (cond
    (symbol? s)
    (swap! symbols conj (resolve s))

    (seqable? s)
    (doseq [s* s]
      (form->var s* symbols))))

(defmacro inspect [& fn-decl]
  (let [vars (atom [])]
    (doseq [s fn-decl]
      (form->var s vars))
    (trace-vars "clj-stack.core" @vars))
  `(clojure.core/defn ~@fn-decl))

(defn do-side-effect [args]
  {:received-args args})

(inspect function-2 [args]
  (do-side-effect args))

(defn function-1 [args]
  {:function-1 args})

(inspect function-3 [args]
         (let [banana (function-2 args)
               abacate (function-1 args)
               maca   (clojure.string/capitalize "minusculo")])
         {:status 200 :body args})

(inspect function-4 [args]
         (function-2 args))

(function-4 {:banana 1})

(function-3 {:banana 1})



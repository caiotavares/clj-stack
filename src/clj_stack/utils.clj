(ns clj-stack.utils
  (:import (clojure.lang Var)))

(defn var->namespace [v]
  (-> v meta :ns ns-name))

(defn namespaced
  ([^Var v]
   (when (var? v)
     (namespaced (var->namespace v) (-> v meta :name))))
  ([ns s]
   (keyword (str ns) (str s))))

(defn matches-namespace? [ns var]
  (some? (re-find (re-pattern (str "^" ns))
                  (-> var meta :ns str))))

(ns clj-stack.utils
  (:import (clojure.lang Var)))

(defn var->namespace [v]
  (-> v meta :ns))

(defn namespaced
  ([^Var v]
   (when (var? v)
     (namespaced (-> v var->namespace ns-name) (-> v meta :name))))
  ([ns s]
   (keyword (str ns) (str s))))

(defn matches-namespace? [ns var]
  (some? (re-find (re-pattern (str "^" ns))
                  (-> var meta :ns str))))

(defmacro tap [v]
  `(do
     (println "Tapping =>" '~v)
     (clojure.pprint/pprint ~v)
     ~v))

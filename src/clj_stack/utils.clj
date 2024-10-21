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

(defn matches-filter? [var ns]
  (some? (re-find (re-pattern (str "^" ns))
                  (-> var meta :ns str))))

(defmacro tap [v]
  `(do
     (println "Tapping =>" '~v)
     (clojure.pprint/pprint ~v)
     ~v))

(defn self? [var node]
  (= (namespaced var) node))

(defn schema? [v]
  (contains? (meta (var-get v)) :skeleton))

(defn load-namespace [var]
  (doseq [path ["src/" "test/"]]
    (try
      (load-file (str path (-> var meta :file)))
      (catch Exception _))))

(defn function? [var]
  (try
    (fn? (var-get var))
    (catch Exception _)))

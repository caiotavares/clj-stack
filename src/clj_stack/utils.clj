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
      (catch Exception _e))))

(defn find-first [pred list]
  (first (filter pred list)))

(defn map-vals [f list]
  (map (fn [[k v]] (f v)) list))

(defn map-keys [f list]
  (map (fn [[k v]] (f k)) list))

(defn val-fn [f]
  (fn [[k v]] (f v)))

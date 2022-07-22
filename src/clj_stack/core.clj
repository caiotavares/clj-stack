(ns clj-stack.core
  (:require [clojure.tools.trace :as trace])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader FileInputStream)))

(def ^:dynamic *stack* (atom {}))

(defn ^:private var->namespace [v]
  (-> v meta :ns ns-name str))

(defn ^:private namespaced
  ([s]
   (when (symbol? s)
     (namespaced (var->namespace (resolve s)) s))
   (when (var? s)
     (namespaced (var->namespace s) (-> s meta :name))))
  ([ns s]
   (keyword (str ns) (str s))))

(defn ^:private matches-namespace? [ns var]
  (some? (re-find (re-pattern (str "^" ns))
                  (-> var meta :ns str))))

(defn ^:private extract-source [v]
  "Stolen from clojure.repl/source-fn and modified to provide a reader unnatached from the RT"
  (when-let [^String filepath (:file (meta v))]
    (when-let [stream (FileInputStream. filepath)]
      (with-open [reader (LineNumberReader. (InputStreamReader. stream))]
        (dotimes [_ (dec (:line (meta v)))] (.readLine reader))
        (let [text (StringBuilder.)
              pbr  (proxy [PushbackReader] [reader]
                     (read [] (let [i (proxy-super read)]
                                (.append text (char i))
                                i)))]
          (if (= :unknown *read-eval*)
            (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
            (read {} (PushbackReader. pbr)))
          (read-string (str text)))))))

(defn ^:private trace-vars* [v]
  (trace/trace-vars v))

(defn ^:private trace-stack []
  (->> @*stack*
       vals
       (map :children)
       flatten
       (mapv trace-vars*)))

(defn ^:private form->var [layer expr ns]
  "Extracts called symbols from a fn definition sexp"
  (cond
    (symbol? expr)
    (when-let [v (resolve expr)]
      (when (and (not (= (namespaced v) layer))
                 (matches-namespace? ns v))
        (swap! *stack* update-in [layer :children] conj v)))

    (seqable? expr)
    (doseq [s* expr]
      (form->var layer s* ns))))

(defn ^:private has-children? [layer result]
  (-> result layer :children seq some?))

(defn ^:private expand-children [layer ns]
  (doseq [child (:children (layer @*stack*))]
    (swap! *stack* update (namespaced child) assoc :children '())
    (form->var (namespaced child) (extract-source child) ns)
    (when (has-children? layer @*stack*)
      (doseq [subchild (:children (layer @*stack*))]
        (expand-children (namespaced subchild) ns)))))

(defn ^:private extract-called-functions [root fn-decl ns]
  (form->var root fn-decl ns)
  (expand-children root ns)
  (trace-stack))

(defmacro deftraced [fn-name & fn-decl]
  (let [doc-string (if (string? (first fn-decl)) (first fn-decl) "")
        fn-form    (if (string? (first fn-decl)) (rest fn-decl) fn-decl)
        filter-ns  (or (:namespace (meta fn-name)) (str *ns*))]
    (extract-called-functions (namespaced *ns* fn-name) fn-decl filter-ns)
    `(do
       (declare ~fn-name)
       (let [f# (fn ~@fn-form)]
         (defn ^:dynamic ~fn-name ~doc-string [& args#]
           (trace/trace-fn-call '~fn-name f# args#))))))

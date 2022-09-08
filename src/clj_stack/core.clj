(ns clj-stack.core
  (:require [clojure.tools.trace]
            [clj-stack.state :as state]
            [clj-stack.utils :as utils]
            [clj-stack.tracing :as tracing]
            [clojure.string :as str])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader FileInputStream)))

(defn ^:private extract-source [v]
  "Based on clojure.repl/source-fn and modified to provide a reader unnatached from the RT (which doesn't work for some reason)"
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

(defn ^:private expression->children [node expr ns]
  "Extracts called symbols from a fn definition sexp"
  (cond
    (symbol? expr)
    (when-let [v (resolve expr)]
      (when (and (not (= (utils/namespaced v) node))
                 (utils/matches-namespace? ns v))
        (state/register-child! node v)))

    (seqable? expr)
    (doseq [s* expr]
      (expression->children node s* ns))))

(defn ^:private expand-children [node level ns]
  "Fetches each child fn source and goes through every symbol, registering nodes in the call tree"
  (doseq [child (state/children node)]
    (state/register-node! (utils/namespaced child) level)
    (expression->children (utils/namespaced child) (extract-source child) ns)
    (when (state/has-children? node)
      (doseq [grandchild (state/children node)]
        (expand-children (utils/namespaced grandchild) (inc level) ns)))))

(defn ^:private traverse-call-tree [root level fn-decl ns]
  (state/register-node! root level)
  (expression->children root fn-decl ns)
  (expand-children root (inc level) ns)
  (tracing/trace-stack))

(defmacro deftraced
  "Replacement for defn, but annotates the static call stack from this fn
  onwards based on a namespace filter. When called, will annotate the in/out
  of each fn into the *stack* atom dynamic var.

  In order to customize the namespace filter, add the :namespace key in the
  metadata map.

  e.g.:
  (deftrace ^{:namespace \"clj-stack.core\"} my-fn [args]
    (do-something args))
  "
  [fn-name & fn-decl]
  (state/clear-stack!)
  (let [doc-string (if (string? (first fn-decl)) (first fn-decl) "")
        fn-form    (if (string? (first fn-decl)) (rest fn-decl) fn-decl)
        filter-ns  (or (:namespace (meta fn-name)) (-> *ns* str (str/split #"\.") first))]
    (traverse-call-tree (utils/namespaced *ns* fn-name) 0 fn-decl filter-ns)
    `(do
       (declare ~fn-name)
       (let [f# (fn ~@fn-form)]
         (defn ^:dynamic ~fn-name ~doc-string [& args#]
           (tracing/traced! '~fn-name f# args#))))))

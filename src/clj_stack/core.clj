(ns clj-stack.core
  (:require [clj-stack.state :as state]
            [clj-stack.tracing :as tracing]
            [clojure.repl :as repl]
            [clj-stack.utils :as utils]
            [clojure.string :as str]
            [clojure.tools.trace])
  (:import (clojure.lang Var)
           (java.io FileInputStream InputStreamReader LineNumberReader PushbackReader)))

(defn ^:private extract-source
  "Based on clojure.repl/source-fn and modified to provide a reader unnatached from the RT (which doesn't work for some reason)"
  [v]
  (when-let [^String filepath (:file (meta v))]
    ;; TODO: Fix filepath absolute reference, right now all namespaces must be loaded to the REPL beforehand
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

(defn ^:private expression->children
  "Extracts called symbols from a fn definition sexp"
  [node ns expr filter]
  (cond
    (symbol? expr)
    (when-let [v (ns-resolve ns expr)]
      (when (and (not (= (utils/namespaced v) node))
                 (utils/matches-namespace? filter v))
        (state/register-child! node v)))

    (seqable? expr)
    (doseq [s* expr]
      (expression->children node ns s* filter))))

(defn ^:private traverse-call-tree
  "Fetches each child fn source and goes through every symbol, registering nodes in the call tree"
  [level node ns source filter]
  (state/register-node! node level)
  (expression->children node ns source filter)
  (when-let [children (state/children node)]
    (doseq [child children]
      (traverse-call-tree (inc level) (utils/namespaced child) (utils/var->namespace child) (repl/source-fn 'child) filter))))

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
        filter     (or (:namespace (meta fn-name)) (-> *ns* str (str/split #"\.") first))
        level      0]
    (traverse-call-tree level (utils/namespaced *ns* fn-name) *ns* fn-decl filter)
    (tracing/trace-stack)
    `(do
       (declare ~fn-name)
       (let [f# (fn ~@fn-form)]
         (defn ^:dynamic ~fn-name ~doc-string [& args#]
           (tracing/traced! '~fn-name f# args#))))))

(defn expand-calls
  "Expands call stack for a given Var"
  [^Var v]
  (state/clear-stack!)
  (let [level  0
        ns     (-> v meta :ns)
        filter (-> ns str (str/split #"\.") first)]
    (traverse-call-tree level (utils/namespaced v) ns (extract-source v) filter)
    @state/*stack*))

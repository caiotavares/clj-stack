(ns clj-stack.core
  (:require [clojure.tools.trace]
            [clj-stack.print :as print])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader FileInputStream)
           (clojure.lang Var)))

(def ^:dynamic *stack* (atom {}))

(defn ^:private new-node [level]
  {:children '()
   :input    nil
   :output   nil
   :level    level})

(defn ^:private register-node! [node level]
  (swap! *stack* assoc-in [node] (new-node level)))

(defn ^:private register-call!
  [node var-name]
  (swap! *stack* update-in [node :children] conj var-name))

(defn ^:private register-input! [node args]
  (print/input! *stack* node args)
  (swap! *stack* update node assoc :input args))

(defn ^:private register-output! [node result]
  (print/output! *stack* node result)
  (swap! *stack* update node assoc :output result))

(defn ^:private var->namespace [v]
  (-> v meta :ns ns-name))

(defn ^:private namespaced
  ([^Var v]
   (when (var? v)
     (namespaced (var->namespace v) (-> v meta :name))))
  ([ns s]
   (keyword (str ns) (str s))))

(defn traced [fn-name fn args]
  (let [node (namespaced (if (symbol? fn-name) (resolve fn-name) fn-name))]
    (register-input! node args)
    (let [result (apply fn args)]
      (register-output! node result)
      result)))

(defn ^:private matches-namespace? [ns var]
  (some? (re-find (re-pattern (str "^" ns))
                  (-> var meta :ns str))))

(defn ^:private extract-source [v]
  "Based on clojure.repl/source-fn and modified to provide a reader unnatached from the RT (which doesn't work for some reason)"
  (when-let [^String filepath (:file (meta v))]
    (when-let [stream (FileInputStream. filepath)]
      (with-open [reader (LineNumberReader. (InputStreamReader. stream))]
        (dotimes [_ (dec (:line (meta v)))] (.readLine reader))
        (let [text (StringBuilder.)
              pbr (proxy [PushbackReader] [reader]
                    (read [] (let [i (proxy-super read)]
                               (.append text (char i))
                               i)))]
          (if (= :unknown *read-eval*)
            (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
            (read {} (PushbackReader. pbr)))
          (read-string (str text)))))))

(defn ^:private trace-vars* [v]
  (alter-var-root v #(fn tracing-wrapper [& args]
                       (traced v % args))))

(defn ^:private trace-stack []
  (->> *stack*
       deref
       vals
       (map :children)
       flatten
       (mapv trace-vars*)))

(defn ^:private expression->var [node expr ns]
  "Extracts called symbols from a fn definition sexp"
  (cond
    (symbol? expr)
    (when-let [v (resolve expr)]
      (when (and (not (= (namespaced v) node))
                 (matches-namespace? ns v))
        (register-call! node v)))

    (seqable? expr)
    (doseq [s* expr]
      (expression->var node s* ns))))

(defn ^:private has-children? [node result]
  (-> result node :children seq some?))

(defn ^:private expand-children [node level ns]
  "Fetches the fn source and goes through every symbol, registering nodes in the call tree"
  (doseq [child (:children (node @*stack*))]
    (register-node! (namespaced child) level)
    (expression->var (namespaced child) (extract-source child) ns)
    (when (has-children? node @*stack*)
      (doseq [subchild (:children (node @*stack*))]
        (expand-children (namespaced subchild) (inc level) ns)))))

(defn ^:private compose-stack [root level fn-decl ns]
  (register-node! root level)
  (expression->var root fn-decl ns)
  (expand-children root (inc level) ns)
  (trace-stack))

(defmacro deftraced
  "Replacement for defn, but annotates the static call stack from this fn
  onwards based on a namespace filter. When called, will annotate the in/out
  of each fn into the *stack* atom dynamic var.

  In order to customize the namespace filter, add the :namespace key in the
  metadata map.

  Ex:
  (deftrace ^{:namespace \"clj-stack.core\"} my-fn [args]
    (do-something args))
  "
  [fn-name & fn-decl]
  (reset! *stack* {})
  (let [doc-string (if (string? (first fn-decl)) (first fn-decl) "")
        fn-form (if (string? (first fn-decl)) (rest fn-decl) fn-decl)
        filter-ns (or (:namespace (meta fn-name)) (str *ns*))]
    (compose-stack (namespaced *ns* fn-name) 0 fn-decl filter-ns)
    `(do
       (declare ~fn-name)
       (let [f# (fn ~@fn-form)]
         (defn ^:dynamic ~fn-name ~doc-string [& args#]
           (traced '~fn-name f# args#))))))

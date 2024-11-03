(ns flux.api
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [flux.core :as core]
            [flux.file :as file]
            [flux.state :as state]
            [flux.defaults :as defaults]
            [flux.tracing :as tracing]
            [flux.utils :as utils])
  (:import (clojure.lang Var)))

(defn ns-filter [ns]
  {:ns-filter (-> ns str (str/split #"\.") first)})

(defn get-stack
  ([] (get-stack {}))
  ([{:keys [layout] :as options :or {layout defaults/layout}}]
   (case layout
     :flat (state/sorted-stack)
     :tree (state/tree-stack (merge defaults/options options)))))

(defn print-stack [options]
  (pprint/pprint (get-stack options)))

(defn expand-stack
  "Expands static call stack for a given Var, works similar to deftraced by without
  redefining the root fn.

  Options should be provided as optional map:

  :ns-filter - only registers fns that match this filter. Defaults to root namespace of the fn
  :trace? - registers input and outputs for each mapped fn (default false)
  :print? - prints the entire stack after the root fn is called (default false)
  :layout - layout to consider when printing stack. Options are :tree (default) or :flat
  :filter-keys - keys to consider when printing elements (default [:name :children])

  e.g.:
  (expand-stack #'my-fn {:filter-keys [:name :children]})"
  ([^Var f] (expand-stack f {}))
  ([^Var f options]
   (state/clear-stack!)
   (let [level 0
         ns    (-> f meta :ns)
         {:keys [ns-filter trace?] :as options} (merge (ns-filter ns) defaults/options options)]
     (core/traverse-call-tree level (utils/namespaced f) ns (file/read-source f) ns-filter)
     (when trace?
       (tracing/trace-var* f)
       (tracing/trace-stack))
     (get-stack (if trace? options (merge options {:filter-keys [:name :children]}))))))

(defn trace-stack
  ([^Var f] (trace-stack f {}))
  ([^Var f options]
   (state/clear-stack!)
   (let [level 0
         ns    (-> f meta :ns)
         {:keys [ns-filter]} (merge (ns-filter ns) defaults/options options)]
     (core/traverse-call-tree level (utils/namespaced f) ns (file/read-source f) ns-filter)
     (tracing/trace-var* f)
     (tracing/trace-stack)
     nil)))

(defmacro deftraced
  "Replacement for defn, but annotates the static call stack from this fn
  onwards based on a namespace filter. If traced, will annotate the in/out
  of each fn into a *stack* atom.

  Options should be provided as metadata keys:

  :ns-filter - only registers fns that match this filter. Defaults to root namespace of the fn
  :trace? - registers input and outputs for each mapped fn (default false)
  :print? - prints the entire stack after the root fn is called (default false)
  :layout - layout to consider when printing stack. Options are :tree (default) or :flat
  :filter-keys - keys to consider when printing elements (default [:name :children :input :output])

  e.g.:
  (deftraced ^{:ns-filter \"flux.core\"} my-fn [args]
    (do-something args))"
  [fn-name & fn-decl]
  (state/clear-stack!)
  (let [level      0
        doc-string (if (string? (first fn-decl)) (first fn-decl) "")
        fn-form    (if (string? (first fn-decl)) (rest fn-decl) fn-decl)
        {:keys [ns-filter trace? print?] :as options} (merge (ns-filter *ns*) defaults/options (meta fn-name))]
    (core/traverse-call-tree level (utils/namespaced *ns* fn-name) *ns* fn-decl ns-filter)
    (when trace? (tracing/trace-stack))
    `(do
       (declare ~fn-name)
       (let [f#   (fn ~@fn-form)
             res# (gensym)]
         (defn ^:dynamic ~fn-name ~doc-string [& args#]
           (let [res# (if ~trace?
                        (tracing/traced! '~fn-name f# args#)
                        (apply f# args#))]
             (when ~print? (print-stack ~options))
             res#))))))

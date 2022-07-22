(ns clj-stack.core
  (:require [clojure.tools.trace :as trace])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader FileInputStream)))

(def stack (atom {}))

(defn var->namespace [v]
  (-> v meta :ns ns-name str))

(defn namespaced
  ([s]
   (when (symbol? s)
     (namespaced (var->namespace (resolve s)) s))
   (when (var? s)
     (namespaced (var->namespace s) (-> s meta :name))))
  ([ns s]
   (keyword (str ns) (str s))))

(defn matches-namespace? [ns var]
  (some? (re-matches (re-pattern (str ns ".?"))
                     (-> var meta :ns str))))

(defn extract-source [v]
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

(defn trace-vars* [v]
  (trace/trace-vars v))

(defn form->var [layer expr ns]
  "Extracts called symbols from a fn definition sexp"
  (cond
    (symbol? expr)
    (when-let [v (resolve expr)]
      (when (and (not (= (namespaced v) layer))
                 (matches-namespace? ns v))
        (swap! stack update-in [layer :children] conj v)))

    (seqable? expr)
    (doseq [s* expr]
      (form->var layer s* ns))))

(comment
  {:clj-stack.core/function-3     {:input    nil
                                   :output   nil
                                   :children '(:clj-stack.core/function-2 :clj-stack.core/function-1)}
   :clj-stack.core/function-2     {:input    nil
                                   :output   nil
                                   :children '(:clj-stack.core/do-side-effect)}
   :clj-stack.core/do-side-effect {:input    nil
                                   :output   nil
                                   :children '(:clj-stack.core/final-function :clj-stack.core/final-final-2)}
   :clj-stack.core/final-function {:input    nil
                                   :output   nil
                                   :children '()}
   :clj-stack.core/final-final-2  {:input    nil
                                   :output   nil
                                   :children '()}
   :clj-stack.core/function-1     {:input    nil
                                   :output   nil
                                   :children '()}})

(defn has-children? [layer result]
  (-> result layer :children seq some?))

(defn expand-children [layer ns]
  (doseq [child (:children (layer @stack))]
    ;; Register child in the map
    (swap! stack update (namespaced child) assoc :children '())
    ;; Expand children from this child
    (form->var (namespaced child) (extract-source child) ns)
    (when (has-children? layer @stack)
      (doseq [subchild (:children (layer @stack))]
        (expand-children (namespaced subchild) ns)))))

(defn extract-called-functions [root fn-decl ns]
  (form->var root fn-decl ns)
  (expand-children root ns)
  (clojure.pprint/pprint @stack))

(defmacro deftraced [fn-name & fn-decl]
  (extract-called-functions (namespaced *ns* fn-name) fn-decl "clj-stack.core")
  `(clojure.core/defn ~fn-name ~@fn-decl))

(defn final-final-2 [args])

(defn final-function [args]
  "do nothing")

(defn do-side-effect [args]
  (final-function {:received-args args})
  (final-final-2 args))

(defn function-1 [args]
  {:function-1 args})

(defn function-2 [args]
  (do-side-effect args))

(deftraced function-3 [args]
  (let [banana  (function-2 args)
        abacate (function-1 args)
        maca    (clojure.string/capitalize "minusculo")])
  {:status 200 :body args})

(function-3 {:banana 1})

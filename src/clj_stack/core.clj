(ns clj-stack.core
  (:require [clojure.tools.trace :as trace])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader FileInputStream)))

(defn namespaced-symbol
  ([s]
   (namespaced-symbol (-> s resolve meta :ns str) s))
  ([ns s]
   (keyword ns (str s))))

(defn namespaced-var [v]
  (let [ns      (-> v meta :ns ns-name str)
        fn-name (-> v meta :name str)]
    (keyword ns fn-name)))

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

(defn trace-vars [ns symbols]
  (->> symbols
       (remove nil?)
       (filter (partial matches-namespace? ns))
       (mapv trace-vars*)))

(defn form->var [layer expr result ns]
  (cond
    (symbol? expr)
    (when-let [v (resolve expr)]
      (when (and (not (= (namespaced-var v) layer))
                 (matches-namespace? ns v))
        (swap! result update-in [layer :children] conj v)))

    (seqable? expr)
    (doseq [s* expr]
      (form->var layer s* result ns))))

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

(defn expand-children [layer result ns]
  (doseq [child (:children (layer @result))]
    ;; Register child in the map
    (swap! result update (namespaced-var child) assoc :children '())
    ;; Expand children from this child
    (form->var (namespaced-var child) (extract-source child) result ns)
    (when (has-children? layer @result)
      (doseq [subchild (:children (layer @result))]
        (expand-children (namespaced-var subchild) result ns)))))

(defn extract-called-functions [root fn-decl ns]
  (let [result (atom {})]
    ;; This is the root function
    (form->var root fn-decl result ns)
    ;; From now on, we need to filter called symbols from the root level
    (expand-children root result ns)
    (nu/tap @result)))

(defmacro deftraced [fn-name & fn-decl]
  (extract-called-functions (namespaced-symbol (str *ns*) fn-name) fn-decl "clj-stack.core")
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

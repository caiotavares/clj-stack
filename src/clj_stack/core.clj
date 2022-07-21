(ns clj-stack.core
  (:require [clojure.tools.trace :as trace])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader FileInputStream)))

(defn namespaced [s]
  (let [ns (-> s resolve meta :ns str)]
    (keyword ns (str s))))

(defn matches-namespace? [ns symbol]
  (some? (re-matches (re-pattern (str ns ".?"))
                     (-> symbol meta :ns str))))

(defn source-fn [v]
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

(defn form->var [layer s symbols]
  (cond
    (symbol? s)
    (when-let [v (resolve s)]
      (swap! symbols update-in [layer] conj v))

    (seqable? s)
    (doseq [s* s]
      (form->var layer s* symbols))))

#_(trace-vars "clj-stack.core" @vars)

(defn extract-called-functions [root fn-decl]
  (let [vars (atom {})]
    ;; This is the root function
    (doseq [s fn-decl]
      (form->var root s vars))
    ;; From now on, we need to filter called symbols from the root level
    (loop [layer 1]
      (nu/tap (str "LAYER - " layer))
      (nu/tap @vars)
      (let [next-level (map source-fn (nu/tap (filter (partial matches-namespace? "clj-stack.core") (get @vars (dec layer)))))]
        (nu/tap "NEXT-LEVEL")
        (nu/tap next-level)
        (when (seq next-level)
          (doseq [s next-level]
            (form->var layer s vars))
          (nu/tap @vars))))))

(defmacro deftraced [name & fn-decl]
  `(clojure.core/defn ~name ~@fn-decl)
  (extract-called-functions (namespaced name) fn-decl))

(defn do-side-effect [args]
  {:received-args args})

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

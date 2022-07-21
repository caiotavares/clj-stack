(ns clj-stack.core
  (:require [clojure.tools.trace :as trace])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader FileInputStream)))

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

(defn form->var [s symbols]
  (cond
    (symbol? s)
    (swap! symbols conj (resolve s))

    (seqable? s)
    (doseq [s* s]
      (form->var s* symbols))))

(defn extract-called-functions [fn-decl]
  (let [vars (atom [])]
    (doseq [s fn-decl]
      (form->var s vars))
    (map source-fn (filter (partial matches-namespace? "clj-stack.core") @vars))
    (trace-vars "clj-stack.core" @vars)))

(defmacro deftraced [name & fn-decl]
  (extract-called-functions fn-decl)
  `(clojure.core/defn ~name ~@fn-decl))

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

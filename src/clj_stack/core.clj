(ns clj-stack.core
  (:import (java.io LineNumberReader InputStreamReader PushbackReader FileInputStream)
           (clojure.lang Var)))

(def ^:dynamic *stack* (atom {}))

(defn tap [f]
  (print "TAP => ")
  (print f)
  (println "")
  f)

(defn ^:private var->namespace [v]
  (-> v meta :ns ns-name))

(defn ^:private namespaced
  ([^Var v]
   (when (var? v)
     (namespaced (var->namespace v) (-> v meta :name))))
  ([ns s]
   (keyword (str ns) (str s))))

(defn traced [fn-name fn args]
  (let [layer (namespaced (if (symbol? fn-name) (resolve fn-name) fn-name))]
    (swap! *stack* update layer assoc :input args)
    (let [result (apply fn args)]
      (swap! *stack* update layer assoc :output result)
      result)))

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
  (->> @*stack*
       vals
       (map :children)
       flatten
       (mapv trace-vars*)))

(defn ^:private expression->var [layer expr ns]
  "Extracts called symbols from a fn definition sexp"
  (cond
    (symbol? expr)
    (when-let [v (resolve expr)]
      (when (and (not (= (namespaced v) layer))
                 (matches-namespace? ns v))
        (swap! *stack* update-in [layer :children] conj v)))

    (seqable? expr)
    (doseq [s* expr]
      (expression->var layer s* ns))))

(defn ^:private has-children? [layer result]
  (-> result layer :children seq some?))

(defn ^:private expand-children [layer ns]
  (doseq [child (:children (layer @*stack*))]
    (swap! *stack* update (namespaced child) assoc :children '())
    (expression->var (namespaced child) (extract-source child) ns)
    (when (has-children? layer @*stack*)
      (doseq [subchild (:children (layer @*stack*))]
        (expand-children (namespaced subchild) ns)))))

(defn ^:private compose-stack [root fn-decl ns]
  (expression->var root fn-decl ns)
  (expand-children root ns)
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
    (compose-stack (namespaced *ns* fn-name) fn-decl filter-ns)
    `(do
       (declare ~fn-name)
       (let [f# (fn ~@fn-form)]
         (defn ^:dynamic ~fn-name ~doc-string [& args#]
           (traced '~fn-name f# args#))))))

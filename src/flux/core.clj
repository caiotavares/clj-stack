(ns flux.core
  (:require [clojure.tools.trace]
            [flux.file :as file]
            [flux.state :as state]
            [flux.utils :as utils]))

(defn ^:private handle-symbol [expr ns node filter]
  (when-let [var (ns-resolve ns expr)]
    (when (and (utils/function? var)
               (not (utils/self? var node))
               (utils/matches-filter? var filter))
      (state/register-child! var node))))

(defn ^:private expression->children
  "Extracts called symbols from a fn definition sexp"
  [node ns expr filter]
  (cond
    (symbol? expr)
    (handle-symbol expr ns node filter)

    (seqable? expr)
    (doseq [s* expr]
      (expression->children node ns s* filter))))

(defn traverse-call-tree
  "Fetches each child fn source and goes through every symbol, registering nodes in the call tree"
  [level node ns source filter]
  (state/register-node! node level)
  (expression->children node ns source filter)
  (when-let [children (map :var (state/list-children node))]
    (doseq [child children]
      (traverse-call-tree (inc level) (utils/namespaced child) (utils/var->namespace child) (file/read-source child) filter))))

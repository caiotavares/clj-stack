(ns clj-stack.state
  (:require [clj-stack.print :as print]))

(def ^:dynamic *stack* (atom {}))

(defn ^:private new-node [level]
  {:children '()
   :input    nil
   :output   nil
   :level    level})

(defn clear-stack! []
  (reset! *stack* {}))

(defn ^:private stack []
  (deref *stack*))

(defn children [node]
  (-> (stack)
      node
      :children))

(defn has-children? [node]
  (-> (children node)
      seq
      some?))

(defn flat-stack []
  (->> (stack)
       vals
       (map :children)
       flatten))

(defn register-node! [node level]
  (swap! *stack* assoc-in [node] (new-node level)))

(defn register-child!
  [node var-name]
  (swap! *stack* update-in [node :children] conj var-name))

(defn register-input! [node args]
  (print/input! *stack* node args)
  (swap! *stack* update node assoc :input args))

(defn register-output! [node result]
  (print/output! *stack* node result)
  (swap! *stack* update node assoc :output result))

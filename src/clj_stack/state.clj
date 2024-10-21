(ns clj-stack.state
  (:require [clj-stack.utils :as utils]))

(def ^:dynamic *stack* (atom {}))
(def call-counter (atom 0))

(defn ^:private new-node [level]
  {:children '()
   :input    nil
   :output   nil
   :throw    nil
   :schema   nil
   :level    level})

(defn ^:private new-child [var]
  {:name (utils/namespaced var)
   :var  var})

(defn clear-stack! []
  (reset! *stack* {})
  (reset! call-counter 0))

(defn ^:private stack []
  (deref *stack*))

(defn children [node]
  (-> (stack)
      node
      :children
      seq))

(defn flat-stack []
  (->> (stack)
       vals
       (map :children)
       flatten
       (map :var)))

(defn find-level [level]
  (->> (stack)
       (filter (fn [[k v]] (= level (:level v))))))

(defn register-node! [node level]
  (swap! *stack* assoc-in [node] (new-node level)))

(defn register-child!
  [var node]
  (swap! *stack* update-in [node :children] conj (new-child var)))

(defn register-schema!
  [schema node]
  (swap! *stack* update node assoc :schema schema))

(defn register-input! [node args]
  (swap! call-counter inc)
  (swap! *stack* update node assoc :input args)
  (swap! *stack* update node assoc :counter @call-counter))

(defn register-output! [node result]
  (swap! *stack* update node assoc :output result))

(defn register-exception! [node ex]
  (let [exception-data (ex-data ex)]
    (swap! *stack* update node assoc :throw exception-data)))

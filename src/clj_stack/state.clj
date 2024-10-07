(ns clj-stack.state
  (:require [clj-stack.utils :as utils]))

(def ^:dynamic *stack* (atom {}))

(defn ^:private new-node [level]
  {:children '()
   :input    nil
   :output   nil
   :throw    nil
   :level    level})

(defn ^:private new-child [var]
  {:name (utils/namespaced var)
   :var  var})

(defn clear-stack! []
  (reset! *stack* {}))

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

(defn register-input! [node args]
  (swap! *stack* update node assoc :input args))

(defn register-output! [node result]
  (swap! *stack* update node assoc :output result))

(defn register-exception! [node ex]
  (let [exception-data (ex-data ex)]
    (swap! *stack* update node assoc :throw exception-data)))

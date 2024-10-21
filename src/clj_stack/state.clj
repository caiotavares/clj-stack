(ns clj-stack.state
  (:require [clj-stack.utils :as utils]))

(def ^:dynamic *stack* (atom {}))
(def ^:dynamic *sequential-stack* (atom {}))

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
  (reset! *stack* {}))

(defn stack []
  (deref *stack*))

(defn children [node]
  (-> (stack)
      node
      :children
      seq))

(defn children-name [node]
  (->> (children node)
       (map #(select-keys % [:name]))))

(defn children-map [node]
  (->> (children-name node)
       (reduce (fn [acc {:keys [name]}] (assoc acc name {:children {}})) {})))

(defn flat-children []
  (->> (stack)
       vals
       (map :children)
       flatten
       (map :var)))

(defn linear-stack []
  (->> (stack)
       (map #())))

(defn find-node [node]
  (get (stack) node))

(defn sorted-stack []
  (sort-by (utils/val-fn #(:level %)) (stack)))

(defn find-level [level]
  (->> (stack)
       (filter (fn [[k v]] (= level (:level v))))))

(defn root []
  (key (first (find-level 0))))

(defn register-node! [node level]
  (swap! *stack* assoc-in [node] (new-node level)))

(defn register-child!
  [var node]
  (swap! *stack* update-in [node :children] conj (new-child var)))

(defn register-schema!
  [schema node]
  (swap! *stack* update node assoc :schema schema))

(defn register-input! [node args]
  (swap! *stack* update node assoc :input args))

(defn register-output! [node result]
  (swap! *stack* update node assoc :output result))

(defn register-exception! [node ex]
  (let [exception-data (ex-data ex)]
    (swap! *stack* update node assoc :throw exception-data)))

(defn ^:private render-children [children path]
  (utils/tap path)
  (doseq [{name :name} children]
    (let [new-path (conj path name :children)]
      (if-let [next (children-name name)]
        (do (swap! *sequential-stack* assoc-in new-path (children-map name))
            (render-children next new-path))
        (swap! *sequential-stack* update-in new-path {:name name :children {}})))))

(defn render-sequential-stack []
  (let [root     (root)
        children (children-name root)]
    (reset! *sequential-stack* {root {:name root :children (children-map root)}})
    (render-children children [root :children])))

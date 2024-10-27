(ns clj-stack.state
  (:require [clj-stack.utils :as utils]))

(def ^:dynamic *stack* (atom {}))

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

(defn flat-children []
  (->> (stack)
       vals
       (map :children)
       flatten
       (map :var)))

(defn find-node [node]
  (get (stack) node))

(defn sorted-stack []
  (sort-by (utils/val-fn #(:level %)) (stack)))

(defn find-level [level]
  (->> (stack)
       (filter (fn [[k v]] (= level (:level v))))))

(defn find-root []
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

(defn ^:private expand-children [current]
  (map (fn [{:keys [name] :as child}]
         (if-let [new-children (children name)]
           (let [updated-map (select-keys (assoc child :children new-children) [:name :children])]
             (update-in updated-map [:children] expand-children))
           (select-keys (assoc child :children []) [:name :children])))
       current))

(defn render-sequential-stack []
  (let [root (find-root)]
    (-> (update-in (stack) [root :children] expand-children)
        (get root)
        (assoc :name root)
        (select-keys [:name :children]))))

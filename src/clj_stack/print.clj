(ns clj-stack.print)

(defn ^:private indent [stack node]
  (let [level (-> stack deref node :level)]
    (apply str (take level (repeat "| ")))))

(defn input! [stack node input]
  (println (indent stack node) "IN  ==> " node " " input))

(defn output! [stack node output]
  (println (indent stack node) "OUT <== " node " " output))

(defn tap [f]
  (print "TAP => ")
  (print f)
  (println "")
  f)

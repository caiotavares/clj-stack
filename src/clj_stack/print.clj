(ns clj-stack.print)

(def regular
  "\033[0m")

(def green
  "\033[38;2;106;168;79;1;m")

(def red
  "\033[38;2;153;0;0;1;m")

(def blue
  "\033[38;2;11;83;148;1;m")

(defn colored [color text]
  (str color text regular))

(def input-color (partial colored green))
(def output-color (partial colored blue))
(def exception-color (partial colored red))

(defn ^:private indent [stack node]
  (let [level (-> stack node :level)]
    (apply str (take level (repeat "| ")))))

(defn input! [stack node input]
  (println green (indent stack node) "IN  ==> " regular node " " input))

(defn output! [stack node output]
  (println blue (indent stack node) "OUT <== " regular node " " output))

(defn exception! [stack node ex]
  (println red (indent stack node) "THROW <== " regular node " " ex))

(defn tap [f]
  (print "TAP => ")
  (print f)
  (println "")
  f)

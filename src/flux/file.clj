(ns flux.file
  (:require [clojure.data.json :as json]
            [flux.state :as state]
            [flux.utils :as utils])
  (:import (java.io FileInputStream InputStreamReader LineNumberReader PushbackReader)))

(defn read-source
  "Based on clojure.repl/source-fn and modified to provide a reader unnatached from the RT (which doesn't work for some reason)"
  [v]
  (utils/load-namespace v)
  (when-let [^String filepath (:file (meta v))]
    ;; TODO: Find-out how to read clj files from the classpath (including JARs)
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

(defn ->file []
  (spit "/tmp/stack.json"
        (json/write-str
          (state/tree-stack {}))))

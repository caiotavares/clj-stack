(ns flux.defaults)

(def trace? false)
(def print? false)
(def layout :tree)
(def filter-keys [:name :children :input :output :throw])

(def options
  {:trace?      trace?
   :print?      print?
   :layout      layout
   :filter-keys filter-keys})

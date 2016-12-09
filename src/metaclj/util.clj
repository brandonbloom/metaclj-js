(ns metaclj.util
  (:require [fipp.edn :refer [pprint]]))

(defmacro change! [v f & args]
  `(set! ~v (~f ~v ~@args)))

(defn dbg [& args]
  (binding [*out* *err*]
    (pprint (list* args))))

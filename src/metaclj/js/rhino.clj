(ns metaclj.js.rhino
  (:refer-clojure :exclude [=])
  (:import (org.mozilla.javascript Context)))

;; See: https://developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino/Embedding_tutorial

(defn enter-context []
  (doto (Context/enter)
    (.setOptimizationLevel -1) ; Disable 64k limit as per cljs implementation.
    (.setLanguageVersion Context/VERSION_ES6)))

(defmacro with-context [name & body]
  `(let [~name (enter-context)]
     (try
       ~@body
       (finally
         (Context/exit)))))

(def scope (with-context ctx (.initStandardObjects ctx)))

(defn eval-string [s]
  (with-context ctx
    (.evaluateString ctx scope s "<clj>" 1 nil)))

(def undefined (org.mozilla.javascript.Undefined/instance))

(defn = [x y]
  (if (or (number? x) (number? y))
    (== x y)
    (clojure.core/= x y)))

(comment
  (eval-string "Object.keys({x:1, y:2})")
)

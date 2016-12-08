(ns metaclj.js.rhino
  (:import (org.mozilla.javascript Context)))

;;XXX context cleanup etc.
;; See: https://developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino/Embedding_tutorial


(defn enter-context []
  (doto (Context/enter)
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

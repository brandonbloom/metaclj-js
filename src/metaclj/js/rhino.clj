(ns metaclj.js.rhino
  (:import (org.mozilla.javascript Context)))

;;XXX context cleanup etc.
;; See: https://developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino/Embedding_tutorial

(def ctx (Context/enter))
(.setLanguageVersion ctx Context/VERSION_ES6)

(def scope (.initStandardObjects ctx))

(defn eval-string [s]
  (.evaluateString ctx scope s "<clj>" 1 nil))

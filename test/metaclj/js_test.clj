(ns metaclj.js-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [metaclj.js :as js :refer [js]])
  (:import (org.mozilla.javascript Context)))

(js/require 'metaclj.js.core :refer :all)

;;XXX context cleanup etc.
;; See: https://developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino/Embedding_tutorial

(def ctx (Context/enter))
(.setLanguageVersion ctx Context/VERSION_ES6)

(def scope (.initStandardObjects ctx))

(defn eval-string [s]
  (.evaluateString ctx scope s "<clj>" 1 nil))

(defmacro eval [form]
  (let [q (with-meta `(js ~form) (meta &form))]
    `(eval-string (doto (with-out-str (js/compile-out ~q)) println))))

(defn js= [x y]
  (if (or (number? x) (number? y))
    (== x y)
    (= x y)))

(deftest eval-test
  (are [form expected] (js= (eval form) expected)

    nil  nil
    "x"  "x"
    1    1
    (* 2 4)  8

    (iife
      (return 1))
    1

    (iife
      (let x 2)
      (return (* x 3)))
    6

    (iife
      (let x 1)
      (set! x 2)
      (return x))
    2

    (iife
      (let x 1)
      (for [(let y 1) (<= y 5) (++ y)]
        (set! x (* x y)))
      (return x))
    120

    ))

(deftest cross-stage-test
  (let [x 1]
    (is (js= (eval x) 1)))
  )


(comment


  (let [n 5]
    (time
      (eval
        (iife
          (let x 1)
          (for [(let y 1) (<= y n) (++ y)]
            (set! x (* x y)))
          (return x)
          ))))

)

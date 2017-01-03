(ns metaclj.js
  (:refer-clojure :exclude [require eval compile])
  (:require [clojure.core :as clj]
            [clojure.spec :as s]
            [metaclj.js.core :as js]
            [metaclj.js.env :as env]
            [metaclj.js.quote :as q]
            [metaclj.js.expand :refer [expand]]
            [metaclj.js.emit :refer [emit]]
            [metaclj.js.rhino :as rhino]))

(env/require-js 'metaclj.js.core :refer :all)

(defn compile-out [x]
  (-> x expand emit))

(defn compile [x]
  (with-out-str
    (compile-out x)))

(defmacro js [& forms]
  (with-meta `(metaclj.js.quote/js ~@forms)
             (meta &form)))

(defn require [ns-sym & filters]
  (apply env/require-js ns-sym filters))

(defmacro eval [form]
  (let [q (with-meta `(js ~form) (meta &form))]
    `(rhino/eval-string (compile ~q))))

(defn quote? [x]
  (q/quote? x))

(ns metaclj.js-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [metaclj.js :as js :refer [js]])
  (:import (org.mozilla.javascript Context)))

(js/require 'metaclj.js.core :refer :all)

(defn js= [x y]
  (if (or (number? x) (number? y))
    (== x y)
    (= x y)))

(deftest eval-test
  (are [form expected] (js= (js/eval form) expected)

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
    (is (js= (js/eval x) 1)))
  )


(comment

  (js/eval (iife (let x 2) (return (* x 3))))

  (let [n 5]
    (time
      (js/eval
        (iife
          (let x 1)
          (for [(let y 1) (<= y n) (++ y)]
            (set! x (* x y)))
          (return x)
          ))))

)

(comment

  (clj/require 'fipp.edn)
  (clj/require 'fipp.clojure)
  (defn party [x]
    ;(->> x :forms first fipp.clojure/pprint)
    ;(->> x :forms first (metaclj.js.parse/parse-in (env/dynamic)) fipp.edn/pprint)
    (->> x expand fipp.edn/pprint)
    ;(->> x expand emit)
    )

  (party (js nil))
  (party (js 1))
  (party (js "foo"))
  (party (js true))
  (party (js (return 1)))
  (party (js js/x))
  (party (let [x 1] (js x)))
  (party (let [x 1] (js ~x)))
  (party (js (* 2 4)))
  (party (js (while (< 1 2) 3 4)))
  (party (js (if ["a" 1])))
  (party (js (if ["a" 1] ["b" 3])))
  (party (js (if ["a" 1 2] ["b" 3 4] 5)))
  (party (js (let x 1)))
  (party (js (for [nil (< 1 2) nil] 3)))
  (party (js (for [(let x 1) (< x 2) (++ x)] 3 4)))
  (party (js (for [(let x 1) (do nil (< x 2)) (++ x)] 3 4)))
  (party (js []))
  (party (js [1 2 3]))
  (party (js {}))
  (party (js {"x" 1}))
  (party (js {"x" 1 "y" 2}))
  (party (js (break)))
  (party (js (continue)))
  (party (js (debugger)))
  (party (js (++ x)))
  (party (js (set! x 1)))
  (party (js (fn [])))
  (party (js (fn [x] x)))
  (party (js (fn [x] (++ x) x)))
  (party (js (fn abc [x y] 1)))
  (party (js (do (let f (fn [x] x)) (f 1))))
  (party (js (inc 2)))
  (party (js (def x 1)))

  (party (js
    (do
      (let x 1)
      (for [(let y 1) (< y 4) (++ y)]
        (set! x (* x y)))
      x)
    ))

  ;; expected parse errors
  (party (js ()))
  (party (js {:x 1}))

  ;; expected environment errors
  (party (js x))
  (party (js (for [(let y 1) (< x 2) (++ x)] 3 4)))

  (alias 'js 'metaclj.js.core)
  (alias 'clj 'clojure.core)
  (env/resolve-js (env/dynamic) 'bogus)
  (env/resolve-js (env/dynamic) 'let)
  (env/resolve-js (env/dynamic) 'bogus/let)
  (env/resolve-js (env/dynamic) 'js/let)
  (env/resolve-js (env/dynamic) 'clj/let)

)

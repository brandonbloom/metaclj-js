(ns metaclj.js-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [metaclj.js :as js :refer [js]]
            [metaclj.js.rhino :as rhino :refer [undefined]]))

(js/require 'metaclj.js.core :refer :all)

(deftest eval-test
  (are [form expected] (rhino/= (js/eval form) expected)

    nil   nil
    "x"   "x"
    1     1
    true  true
    false false

    (* 2 4)  8
    (void 0) undefined

    [] []
    ["x" "y"] ["x" "y"]

    {} {}
    {"x" "a"} {"x" "a"}
    {"x y" "a"} {"x y" "a"}

    ;(iife
    ;  (return))
    ;undefined

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

    (iife
      (let x 2)
      (while (<= x 10)
        (set! x (* x x)))
      (return x))
    16

    (iife
      (if true [(return 1)]))
    1

    (iife
      (if true [(return 1)]
          false [(return 2)]
          [(return 3)]))
    1

    (iife
      (if false [(return 1)]
          true [(return 2)]
          [(return 3)]))
    2

    (iife
      (if false [(return 1)]
          false [(return 2)]
          [(return 3)]))
    3

    (iife
      (if false [(return 1)]
          false [(return 2)]
          [(return 3)]))
    3

    (iife
      (return (cond true 1 2)))
    1

    (iife
      (return (cond false 1 2)))
    2

    (iife
      (let x 0)
      (while true
        (++ x)
        (if
          (< x 3) [(continue)]
          [(break)]))
      (return x))
    3

    (iife
      (if false [(debugger)]))
    undefined

    (iife
      (let x 1)
      (function x [])
      (return x))
    1

    (iife
      (let f
        (function f [x]
          (if (< x 2) [(return (f (+ x 1)))]
            [(return x)])))
      (return (f 0)))
    2

    (iife
      (let g
        (function f [x]
          (if (< x 4) [(return (f (+ x 1)))]
            [(return x)])))
      (return (g 0)))
    4

    ((fn [x y] [y x]) "a" "b")
    ["b" "a"]

    ;(iife
    ;  ;XXX try/catch
    ;  (throw 1))
    ;1

    ))

(deftest cross-stage-test
  (let [x 1]
    (is (rhino/= (js/eval x) 1)))
  (let [x (js 1)]
    (is (rhino/= (js/eval x) 1)))
  (let [xs ["a" "b"]]
    (is (rhino/= (js/eval xs) ["a" "b"])))
  (let [m {"a" "b"}]
    (is (rhino/= (js/eval m) {"a" "b"})))
  )

(deftest meta-symbol-test

  (let [x (gensym)]
    (is (rhino/= (js/eval
                   (iife
                     (let ~x 1)
                     (return x)))
                1)))

   (let [x (gensym)
         a (js (let ~x 2))
         b (js (return x))]
     (is (rhino/= (js/eval (iife a b))
                  2)))

  )


(comment

  (time
    )

)

(comment
  (party (js js/x))
  (party (let [x 1] (js x)))
  (party (let [x 1] (js ~x)))
  (party (js (for [nil (< 1 2) nil] 3)))
  (party (js (for [(let x 1) (< x 2) (++ x)] 3 4)))
  (party (js (for [(let x 1) (do nil (< x 2)) (++ x)] 3 4)))

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

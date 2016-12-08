(ns metaclj.js.core
  (:require [clojure.core :as clj]
            [clojure.spec :as s]
            [metaclj.js.quote :as q :refer [js]]
            [metaclj.js.env :as env :refer [defsyntax]]
            [metaclj.js.operators :as ops]))

(alias 'js 'metaclj.js.core)

(env/declare-specials
  do comma if while ++ for break continue
  return debugger set! let function strict-infix
)

;(defsyntax def (s/cat :name symbol? :init ::js/quote)
;  (env/define-var (:name %) {:head :export})
;  (js (set! (. exports ~(:name %)) (:init %))))

(s/def ::infix-args (s/cat :lhs any? :rhs any?))

(doseq [[sym op] ops/strict-infix]
  (eval (list `env/defsyntax sym ::infix-args
              (list `js (list `strict-infix
                              op
                              (list `unquote (list :lhs '%))
                              (list `unquote (list :rhs '%)))))))

;(doseq [[sym op] ops/short-circuiting]
;  (prn 'short-circuit sym op)) ;XXX

;(doseq [[sym op] ops/assignment]
;  (prn 'assignment sym op)) ;;XXX

;; Creates an "Immediately Invoked Function Expression".
(defsyntax iife (s/* any?)
  (js ((function [] ~@%))))

;; Kinda like the => syntax, but doesn't bind this.
(defsyntax fn (s/cat :args vector? :expr any?)
  (js (function ~(:args %) (return ~(:expr %)))))

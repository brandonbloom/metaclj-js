(ns metaclj.js.emit
  (:require [metaclj.js.core :as js]
            [fipp.engine :refer [pprint-document]]))

;;XXX emit a "'use strict';" somewhere! Safari needs it for tail calls.

(defmulti pretty :head)

;;XXX The ast needs more context!
(defn needs-parens? [ast]
  (case (:head ast)
    (:literal :local) false ;XXX There are others.
    :array false ;XXX Sometimes true?
    (:object :function) true ;XXX Sometimes false!
    true))

(defn pexpr [ast]
  (if (needs-parens? ast)
    [:group "(" (pretty ast) ")"]
    (pretty ast)))

(defn emit [x]
  (-> x pexpr (pprint-document {:width 120})))

(defprotocol Literal
  (-pretty [x]))

(extend-protocol Literal
  nil
  (-pretty [x] "null")
  java.lang.String
  (-pretty [x] (pr-str x)) ;XXX Escaping
  java.lang.Long
  (-pretty [x] (pr-str x))
  java.lang.Double
  (-pretty [x] (pr-str x))
  java.lang.Boolean
  (-pretty [x] (if x "true" "false"))
  clojure.lang.Symbol
  (-pretty [x] (-> x munge str))
  )

(defmethod pretty :literal [{:keys [value]}]
  (-pretty value))

(defmethod pretty :local [{:keys [rename]}]
  (-pretty rename))

(defn statement-class [{:keys [head]}]
  (cond
    (#{`js/for `js/while `js/if `js/function} head) :bodied
    (#{`js/return `js/let `js/set! `js/break `js/continue `js/cond
       `js/++ `js/debugger :invoke :literal `js/strict-infix `js/throw}
     head) :terminated
    :else (throw (ex-info "Unknown statement class" {:head head}))))

(defn pstmt [ast]
  [:span (pretty ast)
         (case (statement-class ast)
           :bodied :break
           :terminated [:span ";" :break])])

(defmethod pretty `js/while [{:keys [test body]}]
  [:group "while (" (pretty test) [:line ""] ") " (pretty body) :break])

(defmethod pretty `js/strict-infix [{:keys [op lhs rhs]}]
  ;;XXX handle precidence & associativity.
  [:group "(" (pretty lhs) " " op " " (pretty rhs) ")"])

(defn pretty-if-clause [{:keys [test then]}]
  [:span "(" (pretty test) ") " (pretty then)])

(defmethod pretty `js/if [{:keys [clauses else]}]
  (let [[test & elifs] clauses]
    [:span "if " (pretty-if-clause test)
           (for [elif elifs]
             [:span " else if " (pretty-if-clause elif)])
           (when else
             [:span " else " (pretty else)])]))

(defmethod pretty `js/cond [{:keys [test then else]}]
  [:span (pexpr test) " ? " (pexpr then) " : " (pexpr else)])

(defmethod pretty `js/++ [{:keys [place]}]
  [:span "++" (pretty place)])

(defmethod pretty `js/return [{:keys [exprs]}]
  [:span "return" (when (seq exprs)
                    (assert (= (count exprs) 1))
                    [:span " " (pexpr (first exprs))])])

(defmethod pretty `js/void [{:keys [expr]}]
  [:span "void " (pexpr expr)])

(defmethod pretty `js/comma [{:keys [exprs]}]
  [:group (interpose [:span "," :line] (map pexpr exprs))])

(defmethod pretty :block [{:keys [stmts]}]
  [:group "{" [:nest :break (map pstmt stmts)] "}"])

(defmethod pretty `js/let [{:keys [sym init]}]
  [:group "let " (-pretty sym) " = " (pexpr init)])

(defmethod pretty `js/for [{:keys [init test step body]}]
  [:group "for (" [:nest [:line ""]
                    (pretty init) ";" :line
                    (pretty test) ";" :line
                    (pretty step) [:line ""]]
          ") " (pretty body)])

(defmethod pretty :array [{:keys [items]}]
  [:group "[" [:line ""]
          [:nest (interpose [:span "," :line]
                            (map pretty items))]
          [:line "" ","]
          "]"])

(defn valid-ident? [x]
  (->> x name (re-matches #"(?i)[a-z_][a-z0-9_]*") boolean))

(defn pretty-key [x]
  (-pretty (if (valid-ident? x)
             (symbol x)
             (name x))))

(defmethod pretty :object [{:keys [items]}]
  [:group "{" [:line ""]
          [:nest (interpose [:span "," :line]
                            (for [[k v] items]
                              [:span (pretty-key k) ": " (pretty v)]))]
          [:line "" ","]
          "}"])

(defmethod pretty `js/break [ast]
  "break")

(defmethod pretty `js/continue [ast]
  "continue")

(defmethod pretty `js/debugger [ast]
  "debugger")

(defmethod pretty `js/set! [{:keys [place init]}]
  [:span (pretty place) " = " (pretty init)])

(defmethod pretty `js/function [{:keys [name params body]}]
  [:group "function" (when name [:span " " (-pretty name)])
          "(" (interpose [:span "," :line] (map -pretty params)) ") "
          (pretty body)])

(defmethod pretty :invoke [{:keys [f args]}]
  [:group (pexpr f) "("
          (interpose [:span "," :line] (map pexpr args))
          ")"])

(defmethod pretty `js/throw [{:keys [expr]}]
  [:span "throw " (pexpr expr)])

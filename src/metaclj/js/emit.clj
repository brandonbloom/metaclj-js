(ns metaclj.js.emit
  (:require [metaclj.js.core :as js]
            [fipp.engine :refer [pprint-document]]))

;;XXX emit a "'use strict';" somewhere!

(defmulti pretty :head)

(defn emit [x]
  (-> x pretty (pprint-document {:width 120})))

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
  (-pretty [x] (str x)) ;XXX munging
  )

(defmethod pretty :literal [{:keys [value]}]
  (-pretty value))

(defmethod pretty :local [{:keys [sym]}]
  (-pretty sym))

(defn expression [ast]
  (if (#{:literal} (:head ast)) ;TODO: Other cases too.
    (pretty ast)
    [:group "(" (pretty ast) ")"]))

(defmethod pretty `js/while [{:keys [test body]}]
  [:group "while (" (pretty test) [:line ""] ")" (pretty body) :break])

(defmethod pretty `js/strict-infix [{:keys [op lhs rhs]}]
  ;;XXX handle precidence & associativity.
  [:group "(" (pretty lhs) " " op " " (pretty rhs) ")"])

(defn pretty-if-clause [{:keys [test then]}]
  [:span " (" (pretty test) ")" (pretty then)])

(defmethod pretty `js/if [{:keys [clauses else]}]
  (let [[test & elifs] clauses]
    [:span "if" (pretty-if-clause test)
           (for [elif elifs]
             [:span " elseif" (pretty-if-clause elif)])
           (when else
             [:span " else" (pretty else)])]))

(defmethod pretty `js/++ [{:keys [place]}]
  [:span "++" (pretty place)])

(defmethod pretty `js/return [{:keys [value]}]
  [:span "return " (expression value)])

(defmethod pretty `js/comma [{:keys [exprs]}]
  [:group (interpose [:span "," :line] (map expression exprs))])

(defmethod pretty :block [{:keys [stmts]}]
  [:group "{" [:nest :break
                     (interleave (map pretty stmts)
                                 (repeat [:span ";" :break]))]
          "}"])

(defmethod pretty :local [{:keys [sym]}]
  (-pretty sym))

(defmethod pretty `js/let [{:keys [sym init]}]
  [:group "let " (-pretty sym) " = " (pretty init)])

(defmethod pretty `js/for [{:keys [init test step body]}]
  [:group "for (" [:nest [:line ""]
                    (pretty init) ";" :line
                    (pretty test) ";" :line
                    (pretty step) [:line ""]]
          ")" (pretty body)])

(defmethod pretty :array [{:keys [items]}]
  [:group "[" [:line ""]
          [:nest (interpose [:span "," :line]
                            (map pretty items))]
          [:line "" ","]
          "]"])

(defmethod pretty :object [{:keys [items]}]
  [:group "{" [:line ""]
          [:nest (interpose [:span "," :line]
                            (for [[k v] items]
                              [:span (-pretty k) ": " (pretty v)]))]
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

(defmethod pretty `js/fn [{:keys [name params body]}]
  [:group "function" (when name [:span " " (-pretty name)])
          "(" (interpose [:span "," :line] (map -pretty params)) ")"
          (pretty body)])

(defmethod pretty :invoke [{:keys [f args]}]
  [:group (expression f) "("
          (interpose [:span "," :line] (map expression args))
          ")"])

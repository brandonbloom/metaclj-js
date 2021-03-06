(ns metaclj.js.emit
  (:require [metaclj.js.core :as js]
            [fipp.engine :refer [pprint-document]]))

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

(defmethod pretty :global [{:keys [name]}]
  (-pretty name))

(defn statement-class [{:keys [head]}]
  (cond
    (#{`js/for `js/while `js/if `js/function} head) :bodied
    (#{:apply :literal :global :member
       `js/return `js/let `js/const `js/set! `js/break `js/continue `js/cond
       `js/++ `js/debugger `js/strict-infix `js/throw `js/instanceof `js/new}
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

(defn pretty-bind [token {:keys [sym init]}]
  [:group token " " (-pretty sym) " = " (pexpr init)])

(defmethod pretty `js/let [ast]
  (pretty-bind "let" ast))

(defmethod pretty `js/const [ast]
  (pretty-bind "const" ast))

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

(defn pretty-args [args]
  [:span "(" (interpose [:span "," :line] (map pexpr args)) ")"])

(defmethod pretty :apply [{:keys [f args]}]
  [:group (pexpr f) (pretty-args args)])

(defmethod pretty `js/throw [{:keys [expr]}]
  [:span "throw " (pexpr expr)])

(defmethod pretty `js/instanceof [{:keys [expr type]}]
  [:span (pexpr expr) " instanceof " (pexpr type)])

(defmethod pretty `js/new [{:keys [ctor args]}]
  [:span "new " (pexpr ctor) (pretty-args args)])

(defmethod pretty :member [{:keys [object property]}]
  [:span (pexpr object) "." (-pretty property)])

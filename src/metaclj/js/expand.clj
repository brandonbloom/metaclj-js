(ns metaclj.js.expand
  (:refer-clojure :exclude [reify eval])
  (:require [clojure.core :as clj]
            [clojure.spec :as s]
            [metaclj.util :refer :all]
            [metaclj.js.core :as js]
            [metaclj.js.parse :refer [parse]]
            [metaclj.js.quote :refer [quote?]]
            [metaclj.js.env :refer [scope] :as env]))

(defn error [msg ast & {:as info}]
  (throw (ex-info (str "expand error: " msg) (merge {:ast ast} info))))

(defn head [x]
  (if (quote? x)
    :quote
    (:head x)))

(defmulti exec-head #'head)

(defn exec
  "Executes a form in the current environment"
  [form]
  (-> form parse exec-head))

(defn eval
  "Executes a form in a sub-environment."
  [form]
  (scope (exec form)))

(defn statement [form]
  (exec form)) ;XXX check result is void?

(defn expression [form]
  (eval form)) ;XXX check result is not void?

(defn block [statements]
  (scope
    {:head :block
     :stmts (mapv exec statements)}))

;;XXX this used to differ from the exec path by returning code, where as
;; the exec path would return a symbol or literal via an ANF transform.
;; move that transform somewhere else because control effects are hard to
;; deal with in direct style, with statements=exprs, etc. Leaving this
;; function though for documentation for a later attempt at this.
(defn reify [x f]
  (f x))

(defn expand [x]
  {:pre [(quote? x)]}
  ;;XXX (multiple!) statements vs expressions
  (eval x))

(defmethod exec-head :literal [ast]
  ast)

(defmethod exec-head :quote [{:keys [forms env] :as ast}]
  (when (not= (count forms) 1)
    (error "splicing quotes not yet implemented" ast)) ;;XXX
  (env/with env
    (-> forms first eval)))

(defmethod exec-head :symbol [{:keys [sym] :as ast}]
  (let [resolved (env/resolve sym)]
    (case (:head resolved)
      :static {:head :literal :value (:value resolved)} ;XXX parse/convert.
      :local resolved
      (error "Unexpected resolve result" ast :resolved resolved))))

(defmethod exec-head :array [ast]
  (update ast :items #(mapv eval %)))

(defn eval-entry [[k v]]
  [k (eval v)])

(defmethod exec-head :object [ast]
  (update ast :items #(mapv eval-entry %)))

(defn eval-if-clause [x]
  (-> x
      (update :test reify expression)
      (update :then reify block)))

(defmethod exec-head `js/if [{:keys [clauses else] :as ast}]
  (-> ast
      (update :clauses #(mapv eval-if-clause %))
      (update :else reify block)))

(defmethod exec-head `js/while [ast]
  (-> ast
      (update :test reify expression)
      (update :body reify block)))

(defn place [p]
  (let [ast (parse p)
        _ (when (not= (:head ast) :symbol)
            (error "non-symbol places not yet implemented" ast)) ;XXX
        x (env/resolve p)]
    (when (not= (:head x) :local) ;XXX
      (error "places other than locals not yet implemented" ast)) ;XXX
    x))

(defmethod exec-head `js/++ [ast]
  (update ast :place place))

(defmethod exec-head `js/for [ast]
  (scope
    (-> ast
        (update :init reify exec)
        (update :test reify eval)
        (update :step reify eval)
        (update :body reify block))))

(defmethod exec-head `js/strict-infix [ast]
  (-> ast
      (update :lhs eval)
      (update :rhs eval)))

(defmethod exec-head `js/do [{:keys [body] :as ast}]
  (block body))

(defmethod exec-head `js/let [{:keys [sym] :as ast}]
  (env/declare-local sym)
  (update ast :init expression))

(defmethod exec-head `js/break [ast]
  ast)

(defmethod exec-head `js/continue [ast]
  ast)

(defmethod exec-head `js/debugger [ast]
  ast)

(defmethod exec-head `js/set! [ast]
  (-> ast
      (update :place place)
      (update :init eval)))

(defmethod exec-head `js/fn [{:keys [name params body] :as ast}]
  (scope
    (run! env/declare-local params)
    (update ast :body reify block)))

(defn expand-macro [{:keys [form] :as ast} {:keys [sym f spec] :as macro}]
  (when-let [ed (s/explain-data spec (next form))]
    (error (str "Call to " sym " did not conform to spec:\n"
                (with-out-str (s/explain-out ed)))
           ast
           :resolved macro
           :explain-data ed))
  (let [expanded (f form env/*env*)]
    (exec expanded)))

(defmethod exec-head :invoke [{:keys [f args form] :as ast}]
  (let [resolved (env/resolve f)]
    (case (:head resolved)
      :clojure (error (str "cannot invoke Clojure var " (:sym resolved)) ast)
      ;XXX :local (-> ast (assoc :f resolved) (update :args #(mapv eval %)))
      :macro (expand-macro ast resolved))))

(defmethod exec-head :apply [ast]
  (-> ast
      (assoc :head :invoke)
      (update :f expression)
      (update :args #(mapv expression %))))

(defmethod exec-head `js/return [{:keys [value]}]
  {:head `js/return :value (expression value)})
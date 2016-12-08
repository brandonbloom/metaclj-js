(ns metaclj.js.parse
  (:require [clojure.core :as clj]
            [clojure.spec :as s]
            [metaclj.js.core :as js]
            [metaclj.js.quote]
            [metaclj.js.env :refer [*env* qualify]]))

(defn error [msg form]
  (throw (ex-info (str "parse error: " msg) {:form form :meta (meta form)})))

(defn conform! [spec form]
  (let [data (s/conform spec form)]
    (if (s/invalid? data)
      (error (s/explain-str spec form) form)
      data)))

(defprotocol Form
  (-parse [x]))

(defn parse [x]
  (assoc (-parse x) :form x :meta (meta x)))

(defn parse-in [env x]
  (binding [*env* env]
    (parse x)))

(defmulti parse-seq first)

(defn parse-apply [form]
  (assoc (conform! (s/cat :f any? :args (s/* any?)) form)
         :head :apply))

(defmethod parse-seq :default [form]
  (assoc (conform! (s/cat :f symbol? :args (s/* any?)) form)
         :head :invoke))

(extend-protocol Form

  java.lang.Object
  (-parse [obj]
    (error (str "unexpected class: " (pr-str (class obj))) obj))

  clojure.lang.ISeq
  (-parse [s]
    (cond
      (empty? s) (error "empty seq" s)
      (symbol? (first s)) (parse-seq (cons (-> s first qualify) (next s)))
      :else (parse-apply s)))

  clojure.lang.IPersistentVector
  (-parse [v]
    {:head :array :items v})

  clojure.lang.IPersistentMap
  (-parse [m]
    {:head :object
     :items (conform! (s/map-of string? any?) m)})

  clojure.lang.Symbol
  (-parse [sym]
    {:head :symbol :sym sym})

  metaclj.js.quote.Quote
  (-parse [x]
    {:head :quote
     :forms (:forms x)
     :env (:env x)})

  )

(defn parse-literal [x]
  {:head :literal :value x})

(doseq [t [nil java.lang.Number java.lang.String java.lang.Boolean]]
  (extend t Form {:-parse parse-literal}))

(defmethod parse-seq `js/return [form]
  (conform! (s/cat :head #{`js/return} :value any?)
            form))

(s/def ::block (s/* any?))

(defmethod parse-seq `js/while [form]
  (conform! (s/cat :head #{`js/while} :test any? :body ::block)
            form))

(s/def ::if-clause (s/and vector?
                          (s/cat :test any? :then ::block)))

(defmethod parse-seq `js/if [form]
  (conform! (s/cat :head #{`js/if}
                   :clauses (s/+ ::if-clause)
                   :else (s/* any?))
            form))

(s/def ::for-control
  (s/and vector?
         (s/cat :init any?  :test any?  :step any?)))

(defmethod parse-seq `js/for [form]
  (let [data (conform! (s/cat :head #{`js/for}
                              :control ::for-control
                              :body ::block)
                       form)]
    (-> data
        (dissoc :control)
        (merge (:control data)))))

(defmethod parse-seq `js/break [form]
  (conform! (s/cat :head #{`js/break}) form))

(defmethod parse-seq `js/continue [form]
  (conform! (s/cat :head #{`js/continue}) form))

(defmethod parse-seq `js/debugger [form]
  (conform! (s/cat :head #{`js/debugger}) form))

(s/def ::place symbol?) ;XXX Support dot access and indexing.

(defmethod parse-seq `js/++ [form]
  (conform! (s/cat :head #{`js/++} :place ::place) form))

(defmethod parse-seq `js/set! [form]
  (conform! (s/cat :head #{`js/set!} :place ::place :init any?) form))

(defmethod parse-seq `js/let [form]
  ;;TODO: destructuring.
  (conform! (s/cat :head #{`js/let} :sym symbol? :init any?) form))

(defmethod parse-seq `js/fn [form]
  (conform! (s/cat :head #{`js/fn}
                   :name (s/? symbol?)
                   :params (s/coll-of symbol? :kind vector?)
                   :body ::block)
          form))

(defmethod parse-seq `js/strict-infix [form]
  (conform! (s/cat :head #{`js/strict-infix}
                   :op string?
                   :lhs any?
                   :rhs any?)
            form))

(defmethod parse-seq `js/do [form]
  (conform! (s/cat :head #{`js/do} :body ::block)
            form))

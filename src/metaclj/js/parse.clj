(ns metaclj.js.parse
  (:require [clojure.core :as clj]
            [clojure.string :as str]
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

(defn parse-apply [[f & args :as form]]
  (if (symbol? f)
    (let [s (name f)]
      (cond
        (str/starts-with? s ".-")
        ,,(do (when (not= (count args) 1)
                (error "Expected one argument to property access" form))
              (when (namespace f)
                (error "Cannot access property of namespace" form))
              {:head :member
               :object (first args)
               :property (symbol (subs s 2))})
        (str/ends-with? s ".")
        ,,(let [sym (symbol (namespace f) (subs s 0 (dec (count s))))]
            (-parse `(js/new ~sym ~@args)))
        (str/starts-with? s ".")
        ,,(if (seq args)
            (let [sym (symbol (str ".-" (subs s 1)))]
              (when (namespace f)
                (error "Cannot call method on namespace" form))
              (-parse `((~sym ~(first args)) ~@(next args))))
            (error "Expected target object for method call"))
        :else {:head :apply :f f :args (vec args)}))
    {:head :apply :f f :args (vec args)}))

(defmethod parse-seq :default [form]
  (assoc (conform! (s/cat :f symbol? :args (s/* any?)) form)
         :head :invoke))

(defn invokeable? [x]
  (and (symbol? x)
       (->> x name (re-find #"\.") nil?)))

(extend-protocol Form

  java.lang.Object
  (-parse [obj]
    (error (str "unexpected class: " (pr-str (class obj))) obj))

  clojure.lang.ISeq
  (-parse [s]
    (cond
      (empty? s) (error "empty seq" s)
      (invokeable? (first s)) (parse-seq (cons (-> s first qualify) (next s)))
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
    (let [[a b] (str/split (name sym) #"\." 2)]
      (if b
        {:head :member
         :object (symbol (namespace sym) a)
         :property (symbol b)}
        {:head :symbol
         :sym (symbol (namespace sym) a)})))

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
  (-> (conform! (s/cat :head #{`js/return}
                       :exprs (s/& (s/* any?) #(<= (count %) 1)))
                form)
      (update :exprs vec)))

(defmethod parse-seq `js/void [form]
  (conform! (s/cat :head #{`js/void} :expr any?)
            form))

(s/def ::block (s/* any?))

(defmethod parse-seq `js/while [form]
  (conform! (s/cat :head #{`js/while} :test any? :body ::block)
            form))

(s/def ::if-clause (s/cat :test any? :then (s/and vector? ::block)))

(defmethod parse-seq `js/if [form]
  (conform! (s/cat :head #{`js/if}
                   :clauses (s/+ ::if-clause)
                   :else (s/? (s/and vector? ::block)))
            form))

(defmethod parse-seq `js/cond [form]
  (conform! (s/cat :head #{`js/cond}
                   :test any?
                   :then any?
                   :else any?)
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

(defmethod parse-seq `js/function [form]
  (conform! (s/cat :head #{`js/function}
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

(defmethod parse-seq `js/throw [form]
  (conform! (s/cat :head #{`js/throw} :expr any?)
            form))

(defmethod parse-seq `js/instanceof [form]
  (conform! (s/cat :head #{`js/instanceof} :expr any? :type any?)
            form))

(defmethod parse-seq `js/new [form]
  (conform! (s/cat :head #{`js/new} :ctor any? :args (s/* any?))
            form))

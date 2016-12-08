(ns metaclj.js.env
  (:refer-clojure :exclude [resolve])
  (:require [clojure.core :as clj]
            [clojure.spec :as s]
            [metaclj.util :refer :all]))

(def ^:dynamic *env*)

(defmacro with [env & body]
  `(binding [*env* ~env]
     ~@body))

(defmacro scope [& body]
  `(with *env* ~@body))

(defrecord Env [ns])

(defonce namespaces (atom {}))

(defn dynamic []
  (if (bound? #'*env*)
    *env*
    (->Env (ns-name *ns*))))

(defmacro lexical []
  `(merge ~(into {}
             (for [sym (keys &env)]
               [(list 'quote sym)
                {:head :static
                 :sym (list 'quote sym)
                 :value sym}]))
          ~(dynamic)))

(defn with-local [env sym x]
  {:pre [(symbol? sym)]}
  (assoc env sym x))

(defn resolve-local [env sym]
  (get env sym))

(defn resolve-js [env sym]
  (let [ns (some-> sym namespace symbol)
        ns (if ns
             (ns-name ((ns-aliases (:ns env)) ns ns))
             (:ns env))
        nm (-> sym name symbol)]
    (some-> (get-in @namespaces [ns nm]) deref)))

(defn resolve-clj [env sym]
  (when-let [v (ns-resolve (:ns env) sym)]
    (let [value @v
          {:keys [ns name]} (meta v)]
      {:head :clojure
       :value value
       :sym (symbol (str ns) (clj/name sym))})))

(defn resolve-in [env sym]
  (assert (:ns env))
  (dbg 'resolve-in env sym)
  (or (resolve-local env sym)
      (resolve-js env sym)
      (resolve-clj env sym)
      (throw (ex-info (str "undefined: " sym)
                      {:sym sym :meta (meta sym)}))))

(defn resolve [sym]
  (resolve-in *env* sym))

(defn qualify [sym]
  (-> sym resolve :sym))

(defn declare-local [sym]
  {:pre [(and (symbol? sym) (nil? (namespace sym)))]}
  (change *env* assoc sym {:head :local :sym sym})
  val)

(defn define-var* [ns sym v]
  (let [qsym (symbol (str ns) (name sym))
        var (assoc v :sym qsym)]
    (swap! namespaces update-in [ns sym]
           #(if %
              (do (reset! % var) %)
              (atom var))))
  nil)

(defn define-var [sym v]
  (assert (nil? (namespace sym)) "cannot define qualified names")
  (define-var* (ns-name *ns*) sym v))

(defmacro declare-specials [& syms]
  (let [ns (-> *ns* ns-name str)]
    `(do ~@(for [sym syms]
             `(define-var '~sym {:head :special}))
         nil)))

(defn define-macro [name spec f]
  (define-var name {:head :macro :f f :spec spec}))

;;TODO: Support docstrings.
(defmacro defsyntax [name spec & body]
  `(define-macro '~name
     ~spec
     (fn ~'[&form &env]
       (let [~'% (s/conform ~spec (next ~'&form))]
         ~@body))))

(s/fdef defsyntax
        :args (s/cat :name symbol? :spec any? :body (s/* any?)))

(defn require-js [ns-sym & filters]
  (clj/require ns-sym)
  (let [this-ns (ns-name *ns*)
        defs (@namespaces ns-sym)
        _ (assert defs (str "No namespace: " ns-sym))
        fs (apply hash-map filters)
        rename (or (:rename fs) {})
        exclude (set (:exclude fs))
        to-do (if (= (:refer fs) :all)
                (keys defs)
                (or (:refer fs) (:only fs) (keys defs)))]
    (assert (or (empty? to-do) (sequential? to-do))
            ":only/:refer value must be a sequential collection of symbols")
    (doseq [sym to-do
            :when (not (exclude sym))
            :let [v (defs sym)]]
      (assert v (str sym " does not exist"))
      (swap! namespaces assoc-in [this-ns (or (rename sym) sym)] v))))

(comment

  (require 'fipp.edn)
  (->
    @namespaces
    ;(get 'clojure.core)
    fipp.edn/pprint
    )

)

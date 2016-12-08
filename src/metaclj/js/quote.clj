(ns metaclj.js.quote
  (:require [clojure.spec :as s]
            [fipp.ednize :as ednize :refer [edn]]
            [backtick :refer [template-fn]]
            [metaclj.js.env :as env]))

(deftype Quote [forms env]
  clojure.lang.ILookup
  (valAt [this key]
    (.valAt this key nil))
  (valAt [this key notFound]
    (case key
      :forms forms
      :env env
      notFound)))

(defn quote? [x]
  (instance? Quote x))


(defn maybe-with-meta [x meta]
  (if (instance? clojure.lang.IObj x)
    (with-meta x meta)
    x))

(defmacro js [& forms]
  (let [loc (-> (meta &form)
                (select-keys [:line :column])
                (assoc :file *file*))
        quotes (for [form forms
                     :let [q (template-fn form)]]
                 `(maybe-with-meta ~q '~(merge loc (meta form))))]
    `(->Quote ~(vec quotes) (env/lexical))))

(extend-protocol ednize/IEdn
  Quote
  (-edn [x]
    (tagged-literal `js {:forms (:forms x) :env (:env x)})))

(defmethod print-method Quote [x ^java.io.Writer writer]
  (print-method (edn x) writer))

(s/def :metaclj.js/quote (s/conformer #(Quote. [%] env/*env*)))

(comment

  (js)
  (js 1)
  (js 1 2 3)
  (let [x 1] (js ~x))
  (js [])

)

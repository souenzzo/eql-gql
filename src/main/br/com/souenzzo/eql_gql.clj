(ns br.com.souenzzo.eql-gql
  (:require [com.walmartlabs.lacinia.parser.query :as lacinia.parser.query]))

(defmulti ->ast #(:type %2))

(defn children
  [env selections]
  (vec (for [selection selections
             children (:children (->ast env selection))]
         children)))

(defmethod ->ast :fragment-definition
  [env {:keys [selections on-type]}]
  {:type     :root
   :children (children (assoc env ::field-ns (name on-type))
                       selections)})

(defmethod ->ast :named-fragment
  [{::keys [fragments] :as env} {:keys [fragment-name]}]
  (->ast env
         (get fragments fragment-name)))

(defmethod ->ast :field
  [{::keys [field-ns key->param->eid] :as env} {:keys [selections args field-name]}]
  (let [dispatch-key (keyword field-ns
                              (name field-name))
        param->eid (get key->param->eid dispatch-key)
        params (when args
                 (into {}
                       (for [{:keys [arg-name arg-value]} args]
                         [arg-name (:value arg-value)])))
        ident? (contains? params param->eid)
        eql-params (dissoc params param->eid)
        params? (if (and ident?
                         (empty? eql-params))
                  false
                  args)]
    {:type     :root
     :children [(cond-> {:type         :prop
                         :key          (if ident?
                                         [dispatch-key (get params param->eid)]
                                         dispatch-key)
                         :dispatch-key dispatch-key}
                        selections (assoc
                                     :type :join
                                     :children (children (assoc env
                                                           ::field-ns (name dispatch-key))
                                                         selections))
                        params? (assoc :params eql-params))]}))

(defmethod ->ast :query
  [{::keys [query->type
            type->ns]
    :as    env} {:keys [selections]}]
  {:type     :root,
   :children (children (assoc env ::field-ns "query")
                       selections)})


(defn query->ast
  [{::keys [query] :as env}]
  (let [els (lacinia.parser.query/parse-query query)
        fragments (into {}
                        (comp (filter :fragment-name)
                              (map (juxt :fragment-name identity)))
                        els)
        query-el (first (filter (comp #{:query} :type)
                                els))]
    (->ast (assoc env
             ::fragments fragments)
           query-el)))

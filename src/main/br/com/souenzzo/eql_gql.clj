(ns br.com.souenzzo.eql-gql
  (:require [com.walmartlabs.lacinia.parser.query :as lacinia.parser.query]
            [com.walmartlabs.lacinia.parser.schema :as parser.schema]))

(defmulti ->ast #(:type %2))

(defn children
  [env selections]
  (vec (for [selection selections
             children (:children (->ast env selection))]
         children)))

(defmethod ->ast :fragment-definition
  [env {:keys [selections on-type] :as type}]
  {:type     :root
   :meta     type
   :children (children (assoc env ::field-ns (name on-type))
                       selections)})

(defmethod ->ast :named-fragment
  [{::keys [fragments] :as env} {:keys [fragment-name]}]
  (->ast env
         (get fragments fragment-name)))

(defn field->type-index
  [{{:keys [objects interfaces]} ::schema} object-id]
  (let [object (merge (get objects object-id)
                      (get interfaces object-id))]
    (into {}
          (for [[id {:keys [type]}] (:fields object)]
            [id (if (list? type)
                  (last type)
                  type)]))))

(defn args->params
  [{::keys [vars]} args]
  (into {}
        (for [{:keys [arg-name arg-value]} args
              :let [type (get arg-value :type)]]
          [arg-name (case type
                      :variable (get vars (get arg-value :value))
                      (:value arg-value))])))

(defn result-from-directives
  [env directives]
  (empty? (for [{:keys [directive-name args]} directives
                :let [vs (args->params env args)]
                {:keys [arg-name]} args
                :let [v (get vs arg-name)
                      remove? (cond
                                (and (= directive-name :include)
                                     (= arg-name :if)
                                     (false? v)) true)]
                :when remove?]
            directive-name)))


(defmethod ->ast :inline-fragment
  [env frag]
  (->ast env (assoc frag :type :fragment-definition)))

(defmethod ->ast :field
  [{::keys [field->type field-ns key->param->eid] :as env}
   {:keys [selections args field-name directives] :as field}]
  (let [object-id (or (get field->type field-name)
                      (throw (ex-info (str "Can't find a type for ':" field-ns "/" (name field-name) "'")
                                      field)))


        dispatch-key (keyword field-ns
                              (name field-name))
        param->eid (get key->param->eid dispatch-key)
        params (when args
                 (args->params env args))
        ident? (contains? params param->eid)
        eql-params (dissoc params param->eid)
        params? (if (and ident?
                         (empty? eql-params))
                  false
                  args)]
    {:type     :root
     :meta     field
     :children (if (result-from-directives env directives)
                 [(cond-> {:type         :prop
                           :key          (if ident?
                                           [dispatch-key (get params param->eid)]
                                           dispatch-key)
                           :dispatch-key dispatch-key}
                          selections (assoc
                                       :type :join
                                       :children (children (assoc env
                                                             ::field->type (field->type-index env object-id)
                                                             ::field-ns (name object-id))
                                                           selections))
                          params? (assoc :params eql-params))]
                 [])}))

(defn ->schema-index
  [schema]
  (let [schema (parser.schema/parse-schema schema {})]
    schema))

(defmethod ->ast :query
  [{{:keys [roots objects]} ::schema
    :as                     env} {:keys [selections vars] :as query}]
  (let [object-id (get roots :query)]
    {:type     :root,
     :meta     query
     :children (children (assoc env
                           ::field->type (field->type-index env object-id)
                           ::vars (merge (into {} (for [{:keys [var-name default]} vars
                                                        :when (contains? default :value)]
                                                    [var-name (:value default)]))
                                         (::vars env))

                           ::field-ns (name object-id))
                         selections)}))



(defmethod ->ast :mutation
  [{{:keys [roots objects]} ::schema
    :as                     env} {:keys [selections vars] :as mutation}]
  (let [object-id (get roots :mutation)
        child (children (assoc env
                          ::field->type (field->type-index env object-id)
                          ::vars (merge (into {} (for [{:keys [var-name default]} vars
                                                       :when (contains? default :value)]
                                                   [var-name (:value default)]))
                                        (::vars env))

                          ::field-ns (name object-id))
                        selections)]
    {:type     :root,
     :meta     mutation
     :children (vec (for [el child]
                      (-> el
                          (assoc :type :call)
                          (update :dispatch-key symbol)
                          (update :key symbol)
                          (update :params #(or % {})))))}))


(defn query->ast
  [{::keys [query] :as env}]
  (let [els (lacinia.parser.query/parse-query query)
        fragments (into {}
                        (comp (filter :fragment-name)
                              (map (juxt :fragment-name identity)))
                        els)
        query-el (first (filter (comp #{:query :mutation} :type)
                                els))]
    (->ast (assoc env
             ::fragments fragments)
           query-el)))

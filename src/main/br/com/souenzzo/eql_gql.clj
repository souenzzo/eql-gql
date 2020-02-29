(ns br.com.souenzzo.eql-gql
  (:require [com.walmartlabs.lacinia.parser.query :as lacinia.parser.query]))

(defn parsed->ast
  [{::keys [fragments placeholder-prefix key->param->eid]
    :as    env}
   {:keys [selections type field-name args] :as el}]
  (let [params (when args
                 (into {}
                       (for [{:keys [arg-name arg-value]} args]
                         [arg-name (:value arg-value)])))
        prefix (if field-name
                 (name field-name)
                 (name type))]
    (cond-> {:type     :root
             :children (vec (for [{:keys [field-name selections fragment-name args] :as field} selections
                                  :let [
                                        frag (get fragments fragment-name)
                                        key (if field-name
                                              (keyword prefix
                                                       (name field-name))
                                              (keyword placeholder-prefix (name fragment-name)))
                                        param->join (get key->param->eid key)
                                        params (when args
                                                 (into {}
                                                       (for [{:keys [arg-name arg-value]} args
                                                             :when (not= arg-name param->join)]
                                                         [arg-name (:value arg-value)])))
                                        params-full (when args
                                                      (into {}
                                                            (for [{:keys [arg-name arg-value]} args]
                                                              [arg-name (:value arg-value)])))
                                        ident? (contains? params-full param->join)]]
                              (cond
                                frag (assoc (parsed->ast env (assoc frag
                                                               :field-name prefix))
                                       :type :join
                                       :key key
                                       :dispatch-key key)
                                selections (assoc (parsed->ast env field)
                                             :type :join
                                             :key (if ident?
                                                    [key (get params-full param->join)]
                                                    key)
                                             :dispatch-key key)
                                :else {:type         :prop
                                       :key          key
                                       :dispatch-key key})))}
            params (assoc :params params))))

(defn query->ast
  [{::keys [query] :as env}]
  (let [els (lacinia.parser.query/parse-query query)
        fragments (into {}
                        (comp (filter :fragment-name)
                              (map (juxt :fragment-name identity)))
                        els)
        query-el (first (filter (comp #{:query} :type)
                                els))]
    (parsed->ast (assoc env
                   ::fragments fragments)
                 query-el)))

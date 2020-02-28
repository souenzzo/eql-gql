(ns br.com.souenzzo.eql-gql
  (:require [com.walmartlabs.lacinia.parser.query :as lacinia.parser.query]))

(defn parsed->ast
  [fragments {:keys [selections type field-name args] :as el}]
  (let [prefix (if field-name
                 (name field-name)
                 (name type))]
    (cond-> {:type     :root
             :children (vec (for [{:keys [field-name selections fragment-name] :as field} selections
                                  :let [frag (get fragments fragment-name)
                                        key (if field-name
                                              (keyword prefix
                                                       (name field-name))
                                              (keyword ">" (name fragment-name)))]]
                              (do

                                (cond
                                  frag (assoc (parsed->ast fragments (assoc frag
                                                                       :field-name prefix))
                                         :type :join
                                         :key key
                                         :dispatch-key key)
                                  selections (assoc (parsed->ast fragments field)
                                               :type :join
                                               :key key
                                               :dispatch-key key)
                                  :else {:type         :prop
                                         :key          key
                                         :dispatch-key key}))))}
            args (assoc :params (into {}
                                      (for [{:keys [arg-name arg-value]} args]
                                        [arg-name (:value arg-value)]))))))

(defn query->ast
  [query]
  (let [els (lacinia.parser.query/parse-query query)
        fragments (into {}
                        (comp (filter :fragment-name)
                              (map (juxt :fragment-name identity)))
                        els)
        query-el (first (filter (comp #{:query} :type)
                                els))]
    (parsed->ast fragments query-el)))

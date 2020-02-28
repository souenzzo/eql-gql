(ns br.com.souenzzo.eql-gql-test
  (:require [br.com.souenzzo.eql-gql :as eql-gql]
            [edn-query-language.core :as eql]
            [clojure.test :refer [deftest is testing]]))

(def pokemon
  "query {
   pokemon(name: \"Pikachu\") {
     id
     number
     name
     attacks {
       special {
         name
         type
         damage
       }
     }
     evolutions {
       id
       number
       name
       weight {
         minimum
         maximum
       }
       attacks {
         fast {
           name
           type
           damage
         }
       }
     }
   }
 }")

(def apollo-frags
  "fragment NameParts on Person {
   firstName
   lastName
 }

 query GetPerson {
   people(id: \"7\") {
     ...NameParts
     avatar(size: LARGE)
   }
 }")


(deftest simple
  (is (= (-> apollo-frags
             eql-gql/query->ast
             eql/ast->query))
      '[({:query/people
          [{:>/NameParts [:people/firstName :people/lastName]}
           :people/avatar]}
         {:id "7"})])
  (is (= (-> "query { User { id name } }"
             eql-gql/query->ast
             eql/ast->query)
         [{:query/User [:User/id
                        :User/name]}]))
  (is (= (-> pokemon
             eql-gql/query->ast
             eql/ast->query)
         '[({:query/pokemon
             [:pokemon/id
              :pokemon/number
              :pokemon/name
              {:pokemon/attacks
               [{:attacks/special
                 [:special/name :special/type :special/damage]}]}
              {:pokemon/evolutions
               [:evolutions/id
                :evolutions/number
                :evolutions/name
                {:evolutions/weight [:weight/minimum :weight/maximum]}
                {:evolutions/attacks
                 [{:attacks/fast [:fast/name :fast/type :fast/damage]}]}]}]}
            {:name "Pikachu"})])))

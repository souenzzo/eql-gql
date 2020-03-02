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

(def schema
  (eql-gql/->schema-index "
type Attac {
  name: String!
  type: String!
  damage: String!
  special: Attac
}
type Pokemon {
  id: String!
  number: String!
  name: String!
  attacks: [Attac]
  evolutions: [Pokemon]
}
type Query {
  me: Person
  people: Person
  pokemon: Pokemon
}

type Person {
  avatar: String!
  firstName: String!
  lastName: String!
}
schema {
  query: Query
}
"))

(deftest simple

  (is (= (-> {::eql-gql/query  "query { me { firstName avatar } }"
              ::eql-gql/schema schema}
             eql-gql/query->ast
             eql/ast->query)
         [{:Query/me [:Person/firstName
                      :Person/avatar]}]))
  (is (= (-> {::eql-gql/query  apollo-frags
              ::eql-gql/schema schema}
             eql-gql/query->ast
             eql/ast->query)
         '[({:Query/people [:Person/firstName
                            :Person/lastName
                            (:Person/avatar {:size :LARGE})]}
            {:id "7"})]))
  (is (= (-> {::eql-gql/query           pokemon
              ::eql-gql/key->param->eid {:query/pokemon :name}
              ::eql-gql/schema          schema}
             eql-gql/query->ast
             eql/ast->query)
         '[{[:query/pokemon "Pikachu"] [:pokemon/id
                                        :pokemon/number
                                        :pokemon/name
                                        {:pokemon/attacks [{:attacks/special [:special/name
                                                                              :special/type
                                                                              :special/damage]}]}
                                        {:pokemon/evolutions [:evolutions/id
                                                              :evolutions/number
                                                              :evolutions/name
                                                              {:evolutions/weight [:weight/minimum
                                                                                   :weight/maximum]}
                                                              {:evolutions/attacks [{:attacks/fast [:fast/name
                                                                                                    :fast/type
                                                                                                    :fast/damage]}]}]}]}])))

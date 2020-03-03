(ns br.com.souenzzo.eql-gql-test
  (:require [br.com.souenzzo.eql-gql :as eql-gql]
            [edn-query-language.core :as eql]
            [clojure.test :refer [deftest is testing]]))

(def pokemon "
query {
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
}
")

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
type Attack {
  name: String
  type: String
  damage: Int
}

type Pokemon {
  id: ID!
  number: String
  name: String
  weight: PokemonDimension
  height: PokemonDimension
  classification: String
  types: [String]
  resistant: [String]
  attacks: PokemonAttack
  weaknesses: [String]
  fleeRate: Float
  maxCP: Int
  evolutions: [Pokemon]
  evolutionRequirements: PokemonEvolutionRequirement
  maxHP: Int
  image: String
}
type PokemonAttack {
  fast: [Attack]
  special: [Attack]
}
type PokemonDimension {
  minimum: String
  maximum: String
}
type PokemonEvolutionRequirement {
  amount: Int
  name: String
}
type Attac {
  name: String!
  type: String!
  damage: String!
  special: Attac
  fast: Attac
}
type Weight {
  minimum: String!
  maximum: String!
}

type CharacterConnection {
  totalCount: String
  edges: CharacterNode
}

type CharacterNode {
  node: Character
}

type Character {
  name: String
  friends: Character
  friendsConnection: CharacterConnection
}

type Query {
  me: Person
  hero: Character
  people: Person
  pokemons(first: Int!): [Pokemon]
  pokemon(id: String, name: String): Pokemon\n
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


(def query-with-vars
  "
  query HeroComparison($first: Int = 3) {
   leftComparison: hero(episode: EMPIRE) {
     ...comparisonFields
   }
   rightComparison: hero(episode: JEDI) {
     ...comparisonFields
   }
 }

 fragment comparisonFields on Character {
   name
   friendsConnection(first: $first) {
     totalCount
     edges {
       node {
         name
       }
     }
   }
 }
 ")

(def query-with-directive
  "query Hero($episode: Episode, $withFriends: Boolean!) {
   hero(episode: $episode) {
     name
     friends @include(if: $withFriends) {
       name
     }
   }
 }")

(def inline-frags
  "query HeroForEpisode($ep: Episode!) {
   hero(episode: $ep) {
     name
     ... on Droid {
       primaryFunction
     }
     ... on Human {
       height
     }
   }
 }")


(deftest simple
  #_(is (= (-> {::eql-gql/query  inline-frags
                ::eql-gql/schema schema}
               eql-gql/query->ast
               eql/ast->query)
           '[({:Query/hero [:Character/name
                            {:Character/friends [:Character/name]}]}
              {:episode :JEDI})]))
  (is (= (-> {::eql-gql/query  query-with-directive
              ::eql-gql/vars   {:episode     :JEDI
                                :withFriends true}
              ::eql-gql/schema schema}
             eql-gql/query->ast
             eql/ast->query)
         '[({:Query/hero [:Character/name
                          {:Character/friends [:Character/name]}]}
            {:episode :JEDI})]))
  (is (= (-> {::eql-gql/query  query-with-vars
              ::eql-gql/schema schema}
             eql-gql/query->ast
             eql/ast->query)
         '[({:Query/hero [:Character/name
                          ({:Character/friendsConnection [:CharacterConnection/totalCount
                                                          {:CharacterConnection/edges [{:CharacterNode/node [:Character/name]}]}]}
                           {:first "3"})]}
            {:episode :EMPIRE})
           ({:Query/hero [:Character/name
                          ({:Character/friendsConnection [:CharacterConnection/totalCount
                                                          {:CharacterConnection/edges [{:CharacterNode/node [:Character/name]}]}]}
                           {:first "3"})]}
            {:episode :JEDI})]))

  (is (= (-> {::eql-gql/query  query-with-vars
              ::eql-gql/vars   {:first 10}
              ::eql-gql/schema schema}
             eql-gql/query->ast
             eql/ast->query)
         '[({:Query/hero [:Character/name
                          ({:Character/friendsConnection [:CharacterConnection/totalCount
                                                          {:CharacterConnection/edges [{:CharacterNode/node [:Character/name]}]}]}
                           {:first 10})]}
            {:episode :EMPIRE})
           ({:Query/hero [:Character/name
                          ({:Character/friendsConnection [:CharacterConnection/totalCount
                                                          {:CharacterConnection/edges [{:CharacterNode/node [:Character/name]}]}]}
                           {:first 10})]}
            {:episode :JEDI})]))

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
              ::eql-gql/key->param->eid {:Query/pokemon :name}
              ::eql-gql/schema          schema}
             eql-gql/query->ast
             eql/ast->query)
         '[{[:Query/pokemon "Pikachu"] [:Pokemon/id
                                        :Pokemon/number
                                        :Pokemon/name
                                        {:Pokemon/attacks [{:PokemonAttack/special [:Attack/name
                                                                                    :Attack/type
                                                                                    :Attack/damage]}]}
                                        {:Pokemon/evolutions [:Pokemon/id
                                                              :Pokemon/number
                                                              :Pokemon/name
                                                              {:Pokemon/weight [:PokemonDimension/minimum
                                                                                :PokemonDimension/maximum]}
                                                              {:Pokemon/attacks [{:PokemonAttack/fast [:Attack/name
                                                                                                       :Attack/type
                                                                                                       :Attack/damage]}]}]}]}])))

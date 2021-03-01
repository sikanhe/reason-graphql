open Jest
open Expect
open Graphql.Language

module Executer = Graphql_Schema_Execution

let starwarsSchema = StarWarsSchema.schema

describe("Basic Queries", () => {
  testAsync("Correctly identifies R2-D2 as the hero of the Star Wars Saga", assertion => {
    let query = `
      query HeroNameQuery {
        hero {
          name
        }
      }
    `

    let expected = Obj.magic({
      "data": {
        "hero": {
          "name": "R2-D2",
        },
      },
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })

  testAsync("Allows us to query for the ID and friends of R2-D2", assertion => {
    let query = `
        query HeroNameAndFriendsQuery {
          hero {
            id
            name
            friends {
              name
            }
          }
        }
    `

    let expected = Obj.magic({
      "data": {
        "hero": {
          "id": 2001,
          "name": "R2-D2",
          "friends": [{"name": "Luke Skywalker"}, {"name": "Han Solo"}, {"name": "Leia Organa"}],
        },
      },
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })
})

describe("Nested Queries", () =>
  testAsync("Allows us to query for the friends of friends of R2-D2", assertion => {
    let query = `
      query NestedQuery {
        hero {
          name
          friends {
            name
            appearsIn
            friends {
              name
            }
          }
        }
      }
    `

    let expected = Obj.magic({
      "data": {
        "hero": {
          "name": "R2-D2",
          "friends": [
            {
              "name": "Luke Skywalker",
              "appearsIn": ["NEWHOPE", "EMPIRE", "JEDI"],
              "friends": [
                {"name": "Han Solo"},
                {"name": "Leia Organa"},
                {"name": "C-3PO"},
                {"name": "R2-D2"},
              ],
            },
            {
              "name": "Han Solo",
              "appearsIn": ["NEWHOPE", "EMPIRE", "JEDI"],
              "friends": [{"name": "Luke Skywalker"}, {"name": "Leia Organa"}, {"name": "R2-D2"}],
            },
            {
              "name": "Leia Organa",
              "appearsIn": ["NEWHOPE", "EMPIRE", "JEDI"],
              "friends": [
                {"name": "Luke Skywalker"},
                {"name": "Han Solo"},
                {"name": "C-3PO"},
                {"name": "R2-D2"},
              ],
            },
          ],
        },
      },
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })
)

describe("Mutation operation", () => {
  open Expect

  let mutation = `
    mutation MyMutation($id: Int!, $name: String!, $appearsIn: String){
      updateCharacterName(characterId: $id, name: $name, appearsIn: $appearsIn) {
        character {
          id
          name
        }
        error
      }
    }
  `

  let variables =
    "{\"id\": 1000, \"name\": \"Sikan Skywalker\"}"
    ->Js.Json.parseExn
    ->Graphql_Json.toVariables
    ->Belt.Result.getExn

  let result =
    starwarsSchema
    |> Executer.execute(
      _,
      ~document=Parser.parse(mutation)->Belt.Result.getExn,
      ~ctx=(),
      ~variables,
    )
    |> Executer.resultToJson

  testAsync("returns the right data", assertion => {
    let expected = Obj.magic({
      "data": {
        "updateCharacterName": {
          "character": {
            "id": 1000,
            "name": "Sikan Skywalker",
          },
          "error": Js.null,
        },
      },
    })

    result |> Executer.Promise.map(res => assertion(expect(res) |> toEqual(expected))) |> ignore
  })
})

describe("Using aliases to change the key in the response", () => {
  open Expect

  testAsync("Allows us to query for Luke, changing his key with an alias", assertion => {
    let query = `
      query FetchLukeAliased {
        luke: human(id: 1000) {
          name
        }
      }
    `

    let expected = Obj.magic({
      "data": {
        "luke": {
          "name": "Luke Skywalker",
        },
      },
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })

  testAsync(
    "Allows us to query for both Luke and Leia, using two root fields and an alias",
    assertion => {
      let query = `
      query FetchLukeAndLeiaAliased {
        luke: human(id: 1000) {
          name
        }
        leia: human(id: 1003) {
          name
        }
      }
    `

      let expected = Obj.magic({
        "data": {
          "luke": {
            "name": "Luke Skywalker",
          },
          "leia": {
            "name": "Leia Organa",
          },
        },
      })

      starwarsSchema
      ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
      ->Executer.resultToJson
      ->Js.Promise.then_(
        res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve,
        _,
      )
      ->ignore
    },
  )
})

describe("Uses fragments to express more complex queries", () => {
  open Expect

  testAsync("Allows us to query using duplicated content", assertion => {
    let query = `
      query DuplicateFields {
        luke: human(id: 1000) {
          name
          homePlanet
        }
        leia: human(id: 1003) {
          name
          homePlanet
        }
      }
    `

    let expected = Obj.magic({
      "data": {
        "luke": {
          "name": "Luke Skywalker",
          "homePlanet": "Tatooine",
        },
        "leia": {
          "name": "Leia Organa",
          "homePlanet": "Alderaan",
        },
      },
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })

  testAsync("Fragment cycle should return error response", assertion => {
    let query = `
    query FragmentCycle {
      f {
        ...A
      }
    }

    fragment A on Human {
      ...B
    }

    fragment B on Driod {
      ...A
    }
  `

    let expected = Obj.magic({
      "data": Js.null,
      "errors": [{"message": "Fragment cycle detected: A -> B", "path": []}],
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })

  testAsync("Allows us to use a fragment to avoid duplicating content", assertion => {
    let query = `
    query UseFragment {
      luke: human(id: 1000) {
        ...HumanFragment
      }
      leia: human(id: 1003) {
        ...HumanFragment
      }
    }

    fragment HumanFragment on Human {
      name
      homePlanet
    }
  `

    let expected = Obj.magic({
      "data": {
        "luke": {
          "name": "Luke Skywalker",
          "homePlanet": "Tatooine",
        },
        "leia": {
          "name": "Leia Organa",
          "homePlanet": "Alderaan",
        },
      },
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })

  testAsync("Allows us to use __typename to get a containing object's type name", assertion => {
    let query = `
    query UseFragment {
      luke: human(id: 1000) {
        __typename
      }
    }
  `

    let expected = Obj.magic({
      "data": {
        "luke": {
          "__typename": "Human",
        },
      },
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })
})

describe("introspection query", () =>
  testAsync("should reply with queries, mutations, and subscriptions", assertion => {
    let query = `
      query IntrospectionQuery {
        __schema {
          queryType {
            name
            fields {
              name
            }
          }
          mutationType {
            name
            fields {
              name
            }
          }
          subscriptionType {
            name
            fields {
              name
            }
          }
        }
      }
    `

    let expected = Obj.magic({
      "data": {
        "__schema": {
          "queryType": {
            "name": "Query",
            "fields": [{"name": "hero"}, {"name": "human"}, {"name": "droid"}],
          },
          "mutationType": {
            "name": "Mutation",
            "fields": [{"name": "updateCharacterName"}],
          },
          "subscriptionType": Js.null,
        },
      },
    })

    starwarsSchema
    ->Executer.execute(~document=Parser.parse(query)->Belt.Result.getExn, ~ctx=())
    ->Executer.resultToJson
    ->Js.Promise.then_(res => assertion(expect(res) |> toEqual(expected)) |> Js.Promise.resolve, _)
    ->ignore
  })
)

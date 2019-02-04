open Jest;
open Expect;
open Language;
let schema = StarWarsSchema.schema;

describe("Basic Queries", () => {
  test("Correctly identifies R2-D2 as the hero of the Star Wars Saga", () => {
    let query = {|
      query HeroNameQuery {
        hero {
          name
        }
      }
    |};

    let result =
      schema
      |> Executor.execute(_, ~document=Parser.parse(query), ~ctx=())
      |> Executor.resultToJson;

    let expected = Executor.{data: `Map([("hero", `Map([("name", `String("R2-D2"))]))])};

    expect(result) |> toEqual(expected->Executor.resultToJson);
  });

  test("Allows us to query for the ID and friends of R2-D2", () => {
    let query = {|
        query HeroNameAndFriendsQuery {
          hero {
            id
            name
            friends {
              name
            }
          }
        }
    |};

    let result =
      schema
      |> Executor.execute(_, ~document=Parser.parse(query), ~ctx=())
      |> Executor.resultToJson;

    let expected =
      Executor.{
        data:
          `Map([
            (
              "hero",
              `Map([
                ("id", `Int(2001)),
                ("name", `String("R2-D2")),
                (
                  "friends",
                  `List([
                    `Map([("name", `String("Luke Skywalker"))]),
                    `Map([("name", `String("Han Solo"))]),
                    `Map([("name", `String("Leia Organa"))]),
                  ]),
                ),
              ]),
            ),
          ]),
      };

    expect(result) |> toEqual(expected->Executor.resultToJson);
  });
});

describe("Nested Queries", () =>
  test("Allows us to query for the friends of friends of R2-D2", () => {
    let query = {|
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
    |};

    let result =
      schema
      |> Executor.execute(_, ~document=Parser.parse(query), ~ctx=())
      |> Executor.resultToJson;

    let expected =
      Executor.{
        data:
          `Map([
            (
              "hero",
              `Map([
                ("name", `String("R2-D2")),
                (
                  "friends",
                  `List([
                    `Map([
                      ("name", `String("Luke Skywalker")),
                      (
                        "appearsIn",
                        `List([`String("NEWHOPE"), `String("EMPIRE"), `String("JEDI")]),
                      ),
                      (
                        "friends",
                        `List([
                          `Map([("name", `String("Han Solo"))]),
                          `Map([("name", `String("Leia Organa"))]),
                          `Map([("name", `String("C-3PO"))]),
                          `Map([("name", `String("R2-D2"))]),
                        ]),
                      ),
                    ]),
                    `Map([
                      ("name", `String("Han Solo")),
                      (
                        "appearsIn",
                        `List([`String("NEWHOPE"), `String("EMPIRE"), `String("JEDI")]),
                      ),
                      (
                        "friends",
                        `List([
                          `Map([("name", `String("Luke Skywalker"))]),
                          `Map([("name", `String("Leia Organa"))]),
                          `Map([("name", `String("R2-D2"))]),
                        ]),
                      ),
                    ]),
                    `Map([
                      ("name", `String("Leia Organa")),
                      (
                        "appearsIn",
                        `List([`String("NEWHOPE"), `String("EMPIRE"), `String("JEDI")]),
                      ),
                      (
                        "friends",
                        `List([
                          `Map([("name", `String("Luke Skywalker"))]),
                          `Map([("name", `String("Han Solo"))]),
                          `Map([("name", `String("C-3PO"))]),
                          `Map([("name", `String("R2-D2"))]),
                        ]),
                      ),
                    ]),
                  ]),
                ),
              ]),
            ),
          ]),
      };

    expect(result) |> toEqual(expected->Executor.resultToJson);
  })
);

describe("Mutation operation", () => {
  open Expect;

  let mutation = {|
    mutation MyMutation($id: Int!, $name: String!){
      updateCharacterName(characterId: $id, name: $name) {
        character {
          id
          name
        }
        error
      }
    }
  |};

  let result =
    schema
    |> Executor.execute(
         _,
         ~document=Parser.parse(mutation),
         ~ctx=(),
         ~variables=[("id", `Int(1000)), ("name", `String("Sikan Skywalker"))],
       )
    |> Executor.resultToJson;

  test("returns the right data", () => {
    let expected =
      Executor.{
        data:
          `Map([
            (
              "updateCharacterName",
              `Map([
                (
                  "character",
                  `Map([("id", `Int(1000)), ("name", `String("Sikan Skywalker"))]),
                ),
                ("error", `Null),
              ]),
            ),
          ]),
      };

    expect(result) |> toEqual(expected->Executor.resultToJson);
  });
});

describe("Using aliases to change the key in the response", () => {
  open Expect;

  test("Allows us to query for Luke, changing his key with an alias", () => {
    let query = {|
      query FetchLukeAliased {
        luke: human(id: 1000) {
          name
        }
      }
    |};

    let result =
      schema
      |> Executor.execute(_, ~document=Parser.parse(query), ~ctx=())
      |> Executor.resultToJson;

    let expected =
      Executor.{data: `Map([("luke", `Map([("name", `String("Luke Skywalker"))]))])};

    expect(result) |> toEqual(expected->Executor.resultToJson);
  });

  test("Allows us to query for both Luke and Leia, using two root fields and an alias", () => {
    let query = {|
      query FetchLukeAndLeiaAliased {
        luke: human(id: 1000) {
          name
        }
        leia: human(id: 1003) {
          name
        }
      }
    |};

    let result =
      schema
      |> Executor.execute(_, ~document=Parser.parse(query), ~ctx=())
      |> Executor.resultToJson;

    let expected =
      Executor.{
        data:
          `Map([
            ("luke", `Map([("name", `String("Luke Skywalker"))])),
            ("leia", `Map([("name", `String("Leia Organa"))])),
          ]),
      };

    expect(result) |> toEqual(expected->Executor.resultToJson);
  });
});

describe("Uses fragments to express more complex queries", () => {
  open Expect;

  test("Allows us to query using duplicated content", () => {
    let query = {|
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
    |};

    let result =
      schema
      |> Executor.execute(_, ~document=Parser.parse(query), ~ctx=())
      |> Executor.resultToJson;

    let expected =
      Executor.{
        data:
          `Map([
            (
              "luke",
              `Map([
                ("name", `String("Luke Skywalker")),
                ("homePlanet", `String("Tatooine")),
              ]),
            ),
            (
              "leia",
              `Map([("name", `String("Leia Organa")), ("homePlanet", `String("Alderaan"))]),
            ),
          ]),
      };

    expect(result) |> toEqual(expected->Executor.resultToJson);
  });

  test("Allows us to use a fragment to avoid duplicating content", () => {
    let query = {|
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
  |};

    let schema = StarWarsSchema.schema;

    let result =
      schema
      |> Executor.execute(_, ~document=Parser.parse(query), ~ctx=())
      |> Executor.resultToJson;

    let expected =
      Executor.{
        data:
          `Map([
            (
              "luke",
              `Map([
                ("name", `String("Luke Skywalker")),
                ("homePlanet", `String("Tatooine")),
              ]),
            ),
            (
              "leia",
              `Map([("name", `String("Leia Organa")), ("homePlanet", `String("Alderaan"))]),
            ),
          ]),
      };

    expect(result) |> toEqual(expected->Executor.resultToJson);
  });
});
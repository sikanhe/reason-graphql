open Jest;
open Language;

describe("Query operation", () => {
  open Expect;

  let query = {|
    query MyQuery($ep: Episode!, $droidId: Int!){
      hero(episode: $ep) {
        id
        name
        appearsIn
      },
      artoo: hero {
        name
      },
      human(id: 1001) {
        id
        name
        homePlanet
      },
      droid(id: $droidId) {
        name
      },
      droidNotFound: droid(id: 999) {
        id
        name
      }
    }
  |};

  let schema = StarWarsSchema.schema;

  let result =
    schema
    |> Execution.execute(
         _,
         ~document=Parser.parse(query),
         ~ctx=(),
         ~variables=[("ep", `String("EMPIRE")), ("droidId", `Int(2000))],
       )
    |> Execution.resultToJson;

  test("returns the right data", () => {
    let expected =
      Execution.{
        data:
          `Map([
            (
              "hero",
              `Map([
                ("id", `Int(1000)),
                ("name", `String("Luke Skywalker")),
                (
                  "appearsIn",
                  `List([`String("NEWHOPE"), `String("JEDI"), `String("EMPIRE")]),
                ),
              ]),
            ),
            ("artoo", `Map([("name", `String("R2-D2"))])),
            (
              "human",
              `Map([
                ("id", `Int(1001)),
                ("name", `String("Darth Vader")),
                ("homePlanet", `String("Tatooine")),
              ]),
            ),
            ("droid", `Map([("name", `String("C-3PO"))])),
            ("droidNotFound", `Null),
          ]),
      };

    expect(result) |> toEqual(expected->Execution.resultToJson);
  });
});

describe("Mutation operation", () => {
  open Expect;

  let mutation = {|
    mutation MyMutation($id: Int!){
      updateCharacter(characterId: $id) {
        id
        name
      }
    }
  |};

  let schema = StarWarsSchema.schema;

  let result =
    schema
    |> Execution.execute(
         _,
         ~document=Parser.parse(mutation),
         ~ctx=(),
         ~variables=[("id", `Int(1000))],
       )
    |> Execution.resultToJson;

  test("returns the right data", () => {
    let expected =
      Execution.{
        data:
          `Map([
            ("updateCharacter", `Map([("id", `Int(1000)), ("name", `String("Luke Skywalker"))])),
          ]),
      };

    expect(result) |> toEqual(expected->Execution.resultToJson);
  });
});
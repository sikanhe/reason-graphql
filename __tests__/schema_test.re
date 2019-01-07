open Jest;
open Language;

describe("Parse and print a graphql query", () => {
  open Expect;

  let query = {|
    {
      hero(episode: EMPIRE) {
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
      droid(id: 2000) {
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
    |> Execution.execute(_, ~document=Parser.parse(query), ~ctx=())
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
            ("droidNotFound", `Null)
          ]),
      };

    expect(result) |> toEqual(expected->Execution.resultToJson);
  });
});
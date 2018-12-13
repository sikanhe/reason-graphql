open Jest;

describe("Parse and print a graphql query", () => {
  open Expect;

  let query = {|
    {
      hero {
        id
        name
        appearsIn
      }
    }
  |};

  let schema = StarWarsSchema.schema;

  let result = schema->Execution.execute(~document=Parser.parse(query));

  test("returns the right data", () => {
    let expected: Ast.constValue =
      `Map([
        (
          "data",
          `Map([
            (
              "hero",
              `Map([
                ("id", `String("1000")),
                ("name", `String("Luke Skywalker")),
                (
                  "appearsIn",
                  `List([`String("NEWHOPE"), `String("JEDI"), `String("EMPIRE")]),
                ),
              ]),
            ),
          ]),
        ),
      ]);

    expect(result) |> toEqual(expected);
  });
});
open Jest;
open Graphql.Language;
module Printer = GrapqhlLanguagePrinter;

describe("Parse and print a graphql query", () => {
  open Expect;

  let query = {|
    query MyQuery($id: Int, $another: [String!]!) { field {
                hello: name
                stringField(name:"\"test\"",nameagain:"汉字")
        # Commentsssss
                age(default: 4.5, a: [4, 5, 5])
        ... on Foo {
                bar(h:{hello:5,nested:{world:1}})
                      ... on BarType {
                baz    @skip(if: $another) @skip(if: $another)
                ...bazFields
        }
        }
      }
    }

    fragment bazFields on FragmentModel {
      foo
      baz {
                g @skip(if: $another)
        ... on Bar {
          # dfffdfdfd
          b @skip(if: $another) @skip(if: {a: [1, 3, 4]})
                  c
            a
        }
      }
    }
 |};

  let document = Parser.parseExn(query);
  let out = document->Printer.print;

  test("Should prettify the query correctly", () => {
    let pretty = {|query MyQuery($id: Int, $another: [String!]!) {
  field {
    hello: name
    stringField(name: "\"test\"", nameagain: "汉字")
    age(default: 4.5, a: [4, 5, 5])
    ... on Foo {
      bar(h: {hello: 5, nested: {world: 1}})
      ... on BarType {
        baz @skip(if: $another) @skip(if: $another)
        ...bazFields
      }
    }
  }
}

fragment bazFields on FragmentModel {
  foo
  baz {
    g @skip(if: $another)
    ... on Bar {
      b @skip(if: $another) @skip(if: {a: [1, 3, 4]})
      c
      a
    }
  }
}|};

    expect(out) |> toBe(pretty);
  });
});
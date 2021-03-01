open Jest
open Graphql_Language

describe("Parse and print a graphql query", () => {
  open Expect

  let query = `
    query My1stQuery($id: Int, $another: [String!]!) { field {
                hello: name
                stringField(name: "test",nameagain:"汉字")
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
 `

  let document = Parser.parse(query)
  let out = document->Belt.Result.getExn->Printer.print

  test("Should prettify the query correctly", () => {
    let pretty = `query My1stQuery($id: Int, $another: [String!]!) {
  field {
    hello: name
    stringField(name: "test", nameagain: "汉字")
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
}`

    expect(out) |> toBe(pretty)
  })
})

describe("Ast mapper", () => {
  let query = `query My1stQuery($id: Int, $another: [String!]!) {
  field {
    hello: name
    stringField(name: "test", nameagain: "汉字")
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
}`

  let expected = `query My1stQuery($id: Int, $another: [String!]!) {
  HI {
    hello: HI
    HI(name: "test", nameagain: "汉字")
    HI(default: 4.5, a: [4, 5, 5])
    ... on Foo {
      HI(h: {hello: 5, nested: {world: 1}})
      ... on BarType {
        HI @skip(if: $another) @skip(if: $another)
        ...bazFields
      }
    }
  }
}

fragment bazFields on FragmentModel {
  HI
  HI {
    HI @skip(if: $another)
    ... on Bar {
      HI @skip(if: $another) @skip(if: {a: [1, 3, 4]})
      HI
      HI
    }
  }
}`

  test("default mapper", () => {
    open Expect

    let document = Parser.parse(query)->Belt.Result.getExn

    let modified = Ast.visit(~enter=Ast.defaultMapper, document)

    let out = Printer.print(modified)

    expect(out) |> toBe(query)
  })

  test("enter", () => {
    open Expect

    let document = Parser.parse(query)->Belt.Result.getExn

    let modified = Ast.visit(
      ~enter={
        ...Ast.defaultMapper,
        field: field => {...field, name: "HI"},
      },
      document,
    )

    let out = Printer.print(modified)

    expect(out) |> toBe(expected)
  })

  test("leave", () => {
    open Expect

    let document = Parser.parse(query)->Belt.Result.getExn

    let modified = Ast.visit(
      ~leave={
        ...Ast.defaultMapper,
        field: field => {...field, name: "HI"},
      },
      document,
    )

    let out = Printer.print(modified)

    expect(out) |> toBe(expected)
  })
})

let query = {|
  query MyQuery($id: Int, $another: [String!]!) { field {
               hello: name
      # Commentsssss
               age(default: 4.5, a: [4, 5, 5])
      ... on Foo {
              bar(h: { hello: 5, nested: { world: 1} })
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

Js.log(Some("dfdf") == Some("abc"));
Js.log(query);
let document = Parser.parse(query);
document->Printer.print->Js.log;
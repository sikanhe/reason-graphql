### Type safe GraphQL server in pure reason. Compiles to nodejs (native soon).

```reason 
open Graphql.Schema; 

type person = {
  name: string,
  age: int,
  children: list(person),
};

let personObject =
  obj("person", ~fields=person =>
    [
      field("name", string, ~resolve=p => p.name),
      field("age", int, ~resolve=p => p.age),
      field("children", List(person), ~resolve=p => p.children),
    ]
  );

let queryType =
  queryType([
    field("random", int, ~resolve=_ => 3),
    field("person", personObject, ~resolve=_ =>
      {name: "sikan", age: 12, children: [{name: "Sikan", age: 2, children: []}]}
    ),
  ]);

let schema = {query: queryType};

let q = {|
  query {
    random
    person {
      name
      age
      children {
        name
        age
      }
    }
  }
|};

let res = schema->Execution.execute(~document=Parser.parse(q));
let json = res->serializeValue;

Js.log(json);
```

[%%debugger.chrome];

open Schema;

let datetime =
  scalar(
    "DateTime",
    ~serialize=date => `String(date->Js.Date.toDateString),
    ~parse=
      input =>
        switch (input) {
        | `String(str) => Js.Date.fromString(str)
        | _ => failwith("Not a valid date")
        },
  );

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
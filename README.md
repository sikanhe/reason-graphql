### Type safe GraphQL server in pure reason. Compiles to nodejs (native soon).

#### TODO: 
- [ ] Language
  - [x] Parser
  - [x] Print Operations 
  - [ ] Print SDL 

- [ ] Schema 
  - [x] Query 
  - [ ] Mutation 
  - [ ] Async Fields
  - [ ] Directives
  - [ ] Subscription
  

```reason
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
  birthday: Js.Date.t,
  children: list(person),
};

let personObject =
  obj("Person", ~fields=person =>
    [
      field("name", string, ~resolve=p => p.name),
      field("age", int, ~resolve=p => p.age),
      field("birthday", datetime, ~resolve=p => p.birthday),
      field("children", List(person), ~resolve=p => p.children),
    ]
  );

let queryType =
  queryType([
    field("random", int, ~resolve=_ => 3),
    field("person", personObject, ~resolve=_ =>
      {
        name: "sikan",
        age: 12,
        birthday: Js.Date.fromString("1993/4/13"),
        children: [
          {name: "Sikan", age: 2, birthday: Js.Date.fromString("1993/4/13"), children: []},
        ],
      }
    ),
  ]);

let schema = {query: queryType};

let q = {|
  query {
    random
    person {
      ...personFields
    }
  }

  fragment personFields on Person {
    name
    age
    birthday
    children {
      name
      age
      birthday
    }
  }
|};

let res = schema->Execution.execute(~document=Parser.parse(q));
let json = res->serializeValue;

Js.log(json);
```

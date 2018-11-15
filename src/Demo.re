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
  birthday: Js.Date.t,
};

let personObject =
  obj("person", ~fields=person =>
    [
      field("name", string, ~resolve=p => p.name),
      field("age", Nullable(int), ~resolve=p => Some(p.age)),
      field("children", List(person), ~resolve=p => p.children),
      field("birthday", datetime, ~resolve=p => p.birthday),
    ]
  );


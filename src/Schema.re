type deprecation =
  | NotDeprecated
  | Deprecated
  | DeprecatedWithReason(string);

type value = [
  | `String(string)
  | `Float(float)
  | `Int(int)
  | `Boolean(bool)
];

type scalar('src) = {
  name: string,
  description: option(string),
  parse: value => 'src,
  serialize: 'src => value,
};

type typ('src) =
  | Scalar(scalar('src)): typ('src)
  | List(typ('src)): typ(list('src))
  | Object(obj('src)): typ('src)
  | Interface(interface('src)): typ('src)
  | Nullable(typ('src)): typ(option('src))
and obj('src) = {
  name: string,
  description: option(string),
  fields: Lazy.t(list(field('src))),
  implements: list(interface('src)),
}
and interface('src) = {
  name: string,
  description: option(string),
  fields: Lazy.t(list(field('src))),
}
and field('src) =
  | Field(fieldDefinition('src, 'out)): field('src)
and fieldDefinition('src, 'out) = {
  name: string,
  typ: typ('out),
  resolve: 'src => 'out,
  description: option(string),
  deprecated: deprecation,
};

let field =
    (~description=None, ~deprecated=NotDeprecated, ~resolve, name, typ) =>
  Field({name, typ, resolve, deprecated, description});

let scalar = (~description=None, ~parse, ~serialize, name) =>
  Scalar({name, description, parse, serialize});

let obj = (~description=None, ~implements=[], ~fields, name) => {
  let rec self =
    Object({name, description, fields: lazy (fields(self)), implements});
  self;
};

let string =
  scalar(
    "String",
    ~serialize=str => `String(str),
    ~parse=
      input =>
        switch (input) {
        | `String(str) => str
        | _ => failwith("Not a string")
        },
  );

let int =
  scalar(
    "Int",
    ~serialize=int => `Int(int),
    ~parse=
      input =>
        switch (input) {
        | `Int(int) => int
        | _ => failwith("Not an integer")
        },
  );

let float =
  scalar(
    "Float",
    ~serialize=float => `Float(float),
    ~parse=
      input =>
        switch (input) {
        | `Float(float) => float
        | _ => failwith("Not a float")
        },
  );

let boolean =
  Scalar({
    name: "Boolean",
    description: None,
    serialize: bool => `Boolean(bool),
    parse: input =>
      switch (input) {
      | `Boolean(bool) => bool
      | _ => failwith("Not a boolean")
      },
  });

let a = "df".[0];

let b = lazy 6;
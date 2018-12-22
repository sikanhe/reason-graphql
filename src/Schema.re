type deprecation =
  | NotDeprecated
  | Deprecated
  | DeprecatedWithReason(string);

let rec serializeValue: Ast.constValue => Js.Json.t =
  fun
  | `String(string) | `Enum(string) => Js.Json.string(string)
  | `Float(float) => Js.Json.number(float)
  | `Int(int) => Js.Json.number(float_of_int(int))
  | `Boolean(bool) => Js.Json.boolean(bool)
  | `List(list) =>
    list |> List.map(item => serializeValue(item)) |> Array.of_list |> Js.Json.array
  | `Map(assocList) => {
      let dict = Js.Dict.empty();
      assocList |> List.iter(((name, value)) => Js.Dict.set(dict, name, serializeValue(value)));
      Js.Json.object_(dict);
    }
  | `Null => Js.Json.null;

type scalar('src) = {
  name: string,
  description: option(string),
  parse: Ast.constValue => 'src,
  serialize: 'src => Ast.constValue,
};

type enum('a) = {
  name: string,
  description: option(string),
  values: list(enumValue('a)),
}
and enumValue('a) = {
  name: string,
  description: option(string),
  deprecated: deprecation,
  value: 'a,
};

type typ('src) =
  | Scalar(scalar('src)): typ('src)
  | Enum(enum('src)): typ('src)
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

type t = {
  query: obj(unit),
  /* mutation: obj(unit), */
};

let create = (~query) => {query: query}; 

let field = (~description=?, ~deprecated=NotDeprecated, name, typ, resolve) =>
  Field({name, typ, resolve, deprecated, description});

let scalar = (~description=?, ~parse, ~serialize, name) =>
  Scalar({name, description, parse, serialize});

let enum = (~description=?, ~values, name) => Enum({
  name,
  description,
  values
});

let enumValue = (~description=?, ~deprecated=NotDeprecated, ~value, name) => {
  name,
  description,
  deprecated,
  value
};

let obj = (~description=?, ~implements=[], ~fields, name) => {
  let rec self = Object({name, description, fields: lazy (fields(self)), implements});
  self;
};

let rootQuery = (fields): obj(unit) => {
  name: "RootQueryType",
  description: None,
  fields: lazy fields,
  implements: [],
};

let string =
  Scalar({
    name: "String",
    description: None,
    serialize: str => `String(str),
    parse: input =>
      switch (input) {
      | `String(str) => str
      | _ => failwith("Not a string")
      },
  });

let int =
  Scalar({
    name: "Int",
    description: None,
    serialize: int => `Int(int),
    parse: input =>
      switch (input) {
      | `Int(int) => int
      | _ => failwith("Not an integer")
      },
  });

let float =
  Scalar({
    name: "Float",
    description: None,
    serialize: float => `Float(float),
    parse: input =>
      switch (input) {
      | `Float(float) => float
      | _ => failwith("Not a float")
      },
  });

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
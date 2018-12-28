type deprecation =
  | NotDeprecated
  | Deprecated
  | DeprecatedWithReason(string);

type enumValue('a) = {
  name: string,
  description: option(string),
  deprecated: deprecation,
  value: 'a,
};

type enum('a) = {
  name: string,
  description: option(string),
  values: list(enumValue('a)),
};

module Arg = {
  type arg(_) =
    | Arg(argument('a)): arg('a)
    | DefaultArg(argumentWithDefault('a)): arg('a)
  and argument('a) = {
    name: string,
    description: option(string),
    typ: argType('a),
  }
  and argumentWithDefault('a) = {
    name: string,
    description: option(string),
    typ: argType(option('a)),
    default: 'a,
  }
  and argType(_) =
    | Scalar(scalar('a)): argType('a)
    | Enum(enum('a)): argType('a)
    | InputObject(inputObject('a, 'b)): argType('a)
    | Nullable(argType('a)): argType(option('a))
    | List(argType('a)): argType(list('a))
  and scalar('a) = {
    name: string,
    description: option(string),
    parse: Ast.constValue => Belt.Result.t('a, string),
  }
  and inputObject('a, 'b) = {
    name: string,
    description: option(string),
    fields: arglist('a, 'b),
    coerce: 'b,
  }
  and arglist(_, _) =
    | []: arglist('a, 'a)
    | ::(arg('a), arglist('b, 'c)): arglist('b, 'a => 'c);

  let arg = (~description=?, name, typ) => Arg({name, typ, description});

  let defaultArg = (~description=?, ~default, name, typ) =>
    DefaultArg({name, typ, description, default});

  /* Built in scalars */

  let string =
    Scalar({
      name: "String",
      description: None,

      parse: input =>
        switch (input) {
        | `String(str) => Ok(str)
        | _ => failwith("Not a string")
        },
    });

  let int =
    Scalar({
      name: "Int",
      description: None,
      parse: input =>
        switch (input) {
        | `Int(int) => Ok(int)
        | _ => failwith("Not an integer")
        },
    });

  let float =
    Scalar({
      name: "Float",
      description: None,
      parse: input =>
        switch (input) {
        | `Float(float) => Ok(float)
        | _ => failwith("Not a float")
        },
    });

  let boolean =
    Scalar({
      name: "Boolean",
      description: None,
      parse: input =>
        switch (input) {
        | `Boolean(bool) => Ok(bool)
        | _ => failwith("Not a boolean")
        },
    });

  let list = a => List(a);
  let nullable = a => Nullable(a);
};

type typ(_) =
  | Scalar(scalar('src)): typ('src)
  | Enum(enum('src)): typ('src)
  | List(typ('src)): typ(list('src))
  | Object(obj('src)): typ('src)
  | Interface(interface('src)): typ('src)
  | Nullable(typ('src)): typ(option('src))
and scalar('src) = {
  name: string,
  description: option(string),
  serialize: 'src => Ast.constValue,
}
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
  | Field(fieldDefinition('src, 'out, 'a, 'args)): field('src)
and fieldDefinition('src, 'out, 'a, 'args) = {
  name: string,
  description: option(string),
  deprecated: deprecation,
  typ: typ('out),
  arguments: Arg.arglist('a, 'args),
  resolve: 'src => 'args,
  lift: 'a => 'out,
};

type t = {
  query: obj(unit),
  /* mutation: obj(unit), */
};

let create = (~query) => {query: query};

let field = (~description=?, ~args, ~deprecated=NotDeprecated, ~typ, ~resolve, name) =>
  Field({name, typ, resolve, deprecated, description, arguments: args, lift: a => a});

type combinedEnum('a) = {
  argType: Arg.argType('a),
  fieldType: typ('a),
};

let makeEnum = (name, ~description=?, values) => {
  argType: Arg.Enum({name, description, values}),
  fieldType: Enum({name, description, values}),
};

let enumValue = (~description=?, ~deprecated=NotDeprecated, ~value, name) => {
  name,
  description,
  deprecated,
  value,
};

let obj = (~description=?, ~implements: list(interface('src))=[], ~fields, name) => {
  let rec self = Object({name, description, fields: lazy (fields(self)), implements});
  self;
};

let rootQuery = (fields): obj(unit) => {
  name: "RootQueryType",
  description: None,
  fields: lazy fields,
  implements: [],
};

/* Built in scalars */
let string = Scalar({name: "String", description: None, serialize: str => `String(str)});
let int = Scalar({name: "Int", description: None, serialize: int => `Int(int)});
let float = Scalar({name: "Float", description: None, serialize: float => `Float(float)});
let boolean = Scalar({name: "Boolean", description: None, serialize: bool => `Boolean(bool)});
let list = a => List(a);
let nullable = a => Nullable(a);
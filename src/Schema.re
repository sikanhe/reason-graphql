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
  and argType(_) =
    | Scalar(scalar('a)): argType('a)
    | Enum(enum('a)): argType('a)
    | InputObject(inputObject('a, 'b)): argType('a)
    | Nullable(argType('a)): argType(option('a))
    | List(argType('a)): argType(list('a))
  and scalar('a) = {
    name: string,
    description: option(string),
    parse: Language.Ast.constValue => Belt.Result.t('a, string),
  }
  and inputObject('a, 'b) = {
    name: string,
    description: option(string),
    fields: arglist('a, 'b),
    coerce: 'b,
  }
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
        | _ => Error("Not a string")
        },
    });

  let int =
    Scalar({
      name: "Int",
      description: None,
      parse: input =>
        switch (input) {
        | `Int(int) => Ok(int)
        | _ => Error("Not an integer")
        },
    });

  let float =
    Scalar({
      name: "Float",
      description: None,
      parse: input =>
        switch (input) {
        | `Float(float) => Ok(float)
        | _ => Error("Not a float")
        },
    });

  let boolean =
    Scalar({
      name: "Boolean",
      description: None,
      parse: input =>
        switch (input) {
        | `Boolean(bool) => Ok(bool)
        | _ => Error("Not a boolean")
        },
    });

  let list = a => List(a);
  let nullable = a => Nullable(a);
};

type typ(_, _) =
  | Scalar(scalar('src)): typ('ctx, 'src)
  | Enum(enum('src)): typ('ctx, 'src)
  | List(typ('ctx, 'src)): typ('ctx, list('src))
  | Object(obj('ctx, 'src)): typ('ctx, 'src)
  | Abstract(abstract): typ('ctx, abstractValue('ctx, 'a))
  | Nullable(typ('ctx, 'src)): typ('ctx, option('src))
and scalar('src) = {
  name: string,
  description: option(string),
  serialize: 'src => Language.Ast.constValue,
}
and obj('ctx, 'src) = {
  name: string,
  description: option(string),
  fields: Lazy.t(list(field('ctx, 'src))),
  abstracts: ref(list(abstract)),
}
and field(_, _) =
  | Field(fieldDefinition('src, 'out, 'ctx, 'a, 'args)): field('ctx, 'src)
and fieldDefinition('src, 'out, 'ctx, 'a, 'args) = {
  name: string,
  description: option(string),
  deprecated: deprecation,
  typ: typ('ctx, 'out),
  args: Arg.arglist('a, 'args),
  resolve: ('ctx, 'src) => 'args,
  lift: 'a => 'out,
}
and anyType =
  | AnyType(typ(_, _)): anyType
  | AnyArgType(Arg.argType(_)): anyType
and abstract = {
  name: string,
  description: option(string),
  mutable types: list(anyType),
  kind: [ | `Union | `Interface(Lazy.t(list(abstractField)))],
}
and abstractField =
  | AbstractField(field(_, _)): abstractField
and abstractValue('ctx, 'a) =
  | AbstractValue((typ('ctx, 'src), 'src)): abstractValue('ctx, 'a);

type abstractType('ctx, 'a) = typ('ctx, abstractValue('ctx, 'a));

type t('ctx) = {
  query: obj('ctx, unit),
  mutation: obj('ctx, unit),
};

type combinedEnum('ctx, 'a) = {
  argType: Arg.argType('a),
  fieldType: typ('ctx, 'a),
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

let obj = (~description=?, ~implements: ref(list(abstract))=ref([]), ~fields, name) => {
  let rec self = Object({name, description, fields: lazy (fields(self)), abstracts: implements});
  self;
};

let field = (~description=?, ~deprecated=NotDeprecated, ~args, ~resolve, name, typ) =>
  Field({name, typ, resolve, deprecated, description, args, lift: a => a});

let abstractField = (~description=?, ~deprecated=NotDeprecated, ~args, name, typ) =>
  AbstractField(
    Field({lift: a => a, name, description, deprecated, typ, args, resolve: Obj.magic()}),
  );

let union = (~description=?, name) => Abstract({name, description, types: [], kind: `Union});

let interface = (~description=?, ~fields, name) => {
  let rec t = Abstract({name, description, types: [], kind: `Interface(lazy (fields(t)))});
  t;
};

let addType = (abstractType, typ) => {
  switch (abstractType, typ) {
  | (Abstract(a), Object(o)) =>
    a.types = [AnyType(typ), ...a.types];
    o.abstracts := [a, ...o.abstracts^];
    (src => AbstractValue((typ, src)));
  | _ => invalid_arg("Arguments must be Interface/Union and Object")
  };
};

let query = (fields): obj('ctx, unit) => {
  name: "Query",
  description: None,
  fields: lazy fields,
  abstracts: ref([]),
};

let mutation = (fields): obj('ctx, unit) => {
  name: "Mutation",
  description: None,
  fields: lazy fields,
  abstracts: ref([]),
};

let create =
    (
      ~mutation={name: "Mutation", description: None, fields: lazy [], abstracts: ref([])},
      query,
    ) => {
  query,
  mutation,
};

/* Built in scalars */
let string = Scalar({name: "String", description: None, serialize: str => `String(str)});
let int = Scalar({name: "Int", description: None, serialize: int => `Int(int)});
let float = Scalar({name: "Float", description: None, serialize: float => `Float(float)});
let boolean = Scalar({name: "Boolean", description: None, serialize: bool => `Boolean(bool)});
let list = a => List(a);
let nullable = a => Nullable(a);
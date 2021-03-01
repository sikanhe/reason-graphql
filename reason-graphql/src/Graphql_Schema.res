open Graphql_Language
module Promise = Js.Promise

type deprecation =
  | NotDeprecated
  | Deprecated(option<string>)

type enumValue<'a> = {
  name: string,
  description: option<string>,
  deprecated: deprecation,
  value: 'a,
}

type enum<'a> = {
  name: string,
  description: option<string>,
  values: list<enumValue<'a>>,
}

module Arg = {
  type rec arg<_> =
    | Arg(argument<'a>): arg<'a>
    | DefaultArg(argumentWithDefault<'a>): arg<'a>
  and argTyp<_> =
    | Scalar(scalar<'a>): argTyp<option<'a>>
    | Enum(enum<'a>): argTyp<option<'a>>
    | InputObject(inputObject<'args, 'a>): argTyp<option<'a>>
    | List(argTyp<'a>): argTyp<option<array<'a>>>
    | NonNull(argTyp<option<'a>>): argTyp<'a>
  and scalar<'a> = {
    name: string,
    description: option<string>,
    parse: Ast.constValue => result<'a, string>,
  }
  and inputObject<'args, 'value> = {
    name: string,
    description: option<string>,
    fields: argList<'args>,
    coerce: (. 'args) => 'value,
  }
  and argument<'a> = {
    name: string,
    description: option<string>,
    typ: argTyp<'a>,
  }
  and argumentWithDefault<'a> = {
    name: string,
    description: option<string>,
    typ: argTyp<option<'a>>,
    default: 'a,
  }

  and argList<'args> =
    | Empty: argList<unit>
    | Arg1(arg<'a>): argList<'a>
    | Arg2(arg<'a>, arg<'b>): argList<('a, 'b)>
    | Arg3(arg<'a>, arg<'b>, arg<'c>): argList<('a, 'b, 'c)>

  let arg = (~description=?, name, typ) => Arg({name: name, typ: typ, description: description})

  let defaultArg = (~description=?, ~default, name, typ) => DefaultArg({
    name: name,
    typ: typ,
    description: description,
    default: default,
  })

  /* Built in scalars */

  let string = Scalar({
    name: "String",
    description: None,
    parse: input =>
      switch input {
      | #String(str) => Ok(str)
      | _ => Error("Invalid string")
      },
  })

  let int = Scalar({
    name: "Int",
    description: None,
    parse: input =>
      switch input {
      | #Int(int) => Ok(int)
      | _ => Error("Invalid integer")
      },
  })

  let float = Scalar({
    name: "Float",
    description: None,
    parse: input =>
      switch input {
      | #Float(float) => Ok(float)
      | _ => Error("Invalid float")
      },
  })

  let boolean = Scalar({
    name: "Boolean",
    description: None,
    parse: input =>
      switch input {
      | #Boolean(bool) => Ok(bool)
      | _ => Error("Invalid boolean")
      },
  })

  let list = a => List(a)
  let nonnull = a => NonNull(a)
}

type rec typ<_, _> =
  | Scalar(scalar<'src>): typ<'ctx, option<'src>>
  | Enum(enum<'src>): typ<'ctx, option<'src>>
  | List(typ<'ctx, 'src>): typ<'ctx, option<array<'src>>>
  | Object(obj<'ctx, 'src>): typ<'ctx, option<'src>>
  | Abstract(abstract): typ<'ctx, option<abstractValue<'ctx, 'a>>>
  | NonNull(typ<'ctx, option<'src>>): typ<'ctx, 'src>
and scalar<'src> = {
  name: string,
  description: option<string>,
  serialize: 'src => Ast.constValue,
}
and obj<'ctx, 'src> = {
  name: string,
  description: option<string>,
  fields: Lazy.t<list<field<'ctx, 'src>>>,
  abstracts: ref<list<abstract>>,
}
and field<_, _> =
  | Field({
      name: string,
      description: option<string>,
      deprecated: deprecation,
      typ: typ<'ctx, 'out>,
      args: Arg.argList<'args>,
      resolve: ('ctx, 'src, 'args) => Promise.t<'out>,
    }): field<'ctx, 'src>
and anyTyp =
  | AnyTyp(typ<_, _>): anyTyp
  | AnyArgTyp(Arg.argTyp<_>): anyTyp
and abstract = {
  name: string,
  description: option<string>,
  types: array<anyTyp>,
  kind: [#Union | #Interface(Lazy.t<array<abstractField>>)],
}
and abstractField = AbstractField(field<_, _>): abstractField
and abstractValue<'ctx, 'a> =
  AbstractValue((typ<'ctx, option<'src>>, 'src)): abstractValue<'ctx, 'a>

type abstractType<'ctx, 'a> = typ<'ctx, option<abstractValue<'ctx, 'a>>>

type directiveLocation = [
  | #Query
  | #Mutation
  | #Subscription
  | #Field
  | #FragmentDefinition
  | #FragmentSpread
  | #InlineFragment
  | #VariableDefinition
]

type directiveInfo<'args> = {
  name: string,
  description: option<string>,
  locations: list<directiveLocation>,
  args: Arg.argList<'args>,
  resolve: 'args => [#Skip | #Include],
}

type rec directive = Directive(directiveInfo<'args>): directive

let skipDirective = Directive({
  name: "skip",
  description: Some(
    "Directs the executor to skip this field or fragment when the `if` argument is true.",
  ),
  locations: list{#Field, #FragmentSpread, #InlineFragment},
  args: {
    open Arg
    Arg1(arg("if", nonnull(boolean), ~description="Skipped when true."))
  },
  resolve: x =>
    switch x {
    | true => #Skip
    | false => #Include
    },
})

let includeDirective = Directive({
  name: "include",
  description: Some(
    "Directs the executor to include this field or fragment only when the `if` argument is true.",
  ),
  locations: list{#Field, #FragmentSpread, #InlineFragment},
  args: {
    open Arg
    Arg1(arg("if", nonnull(boolean), ~description="Included when true."))
  },
  resolve: x =>
    switch x {
    | true => #Include
    | false => #Skip
    },
})

type schema<'ctx> = {
  query: obj<'ctx, unit>,
  mutation: option<obj<'ctx, unit>>,
}

type combinedEnum<'ctx, 'a> = {
  argTyp: Arg.argTyp<'a>,
  fieldType: typ<'ctx, 'a>,
}

let makeEnum = (name, ~description=?, values) => {
  argTyp: Arg.Enum({name: name, description: description, values: Belt.List.fromArray(values)}),
  fieldType: Enum({name: name, description: description, values: Belt.List.fromArray(values)}),
}

let enumValue = (~description=?, ~deprecated=NotDeprecated, ~value, name) => {
  name: name,
  description: description,
  deprecated: deprecated,
  value: value,
}

let obj = (~description=?, ~implements: list<abstract>=list{}, ~fields, name) => {
  let rec self = Object({
    name: name,
    description: description,
    fields: lazy (fields(self)->Belt.List.fromArray),
    abstracts: ref(implements),
  })
  self
}

let field = (~description=?, ~deprecated=NotDeprecated, ~args, ~resolve, name, typ) => Field({
  name: name,
  typ: typ,
  resolve: (ctx, parent, args) => Promise.resolve(resolve(ctx, parent, args)),
  deprecated: deprecated,
  description: description,
  args: args,
})

let asyncField = (~description=?, ~deprecated=NotDeprecated, ~args, ~resolve, name, typ) => Field({
  name: name,
  typ: typ,
  resolve: resolve,
  deprecated: deprecated,
  description: description,
  args: args,
})

let abstractField = (~description=?, ~deprecated=NotDeprecated, ~args, name, typ) => AbstractField(
  Field({
    name: name,
    description: description,
    deprecated: deprecated,
    typ: typ,
    args: args,
    resolve: Obj.magic(),
  }),
)

let union = (~description=?, name) => Abstract({
  name: name,
  description: description,
  types: [],
  kind: #Union,
})

let interface = (~description=?, ~fields, name) => {
  let rec t = Abstract({
    name: name,
    description: description,
    types: [],
    kind: #Interface(lazy fields(t)),
  })
  t
}

let addType = (abstractType, typ) =>
  switch (abstractType, typ) {
  | (Abstract(a), Object(o)) =>
    a.types->Js.Array2.push(AnyTyp(typ))->ignore
    o.abstracts := list{a, ...o.abstracts.contents}
    src => AbstractValue((typ, src))
  | _ => invalid_arg("Arguments must be Interface/Union and Object")
  }

let query = (fields): obj<'ctx, unit> => {
  name: "Query",
  description: None,
  fields: lazy fields,
  abstracts: ref(list{}),
}

let mutation = (fields): obj<'ctx, unit> => {
  name: "Mutation",
  description: None,
  fields: lazy fields,
  abstracts: ref(list{}),
}

let create = (~mutation=?, query) => {query: query, mutation: mutation}

/* Built in scalars */
let string: 'ctx. typ<'ctx, option<string>> = Scalar({
  name: "String",
  description: None,
  serialize: str => #String(str),
})
let int: 'ctx. typ<'ctx, option<int>> = Scalar({
  name: "Int",
  description: None,
  serialize: int => #Int(int),
})
let float: 'ctx. typ<'ctx, option<float>> = Scalar({
  name: "Float",
  description: None,
  serialize: float => #Float(float),
})
let boolean: 'ctx. typ<'ctx, option<bool>> = Scalar({
  name: "Boolean",
  description: None,
  serialize: bool => #Boolean(bool),
})
let list = typ => List(typ)
let nonnull = typ => NonNull(typ)

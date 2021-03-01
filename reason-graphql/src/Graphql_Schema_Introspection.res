open Graphql_Schema
open Belt

/* anyTyp, anyField and anyArg hide type parameters to avoid scope escaping errors */
type rec anyField =
  | AnyField(field<_, _>): anyField
  | AnyArgField(Arg.arg<_>): anyField
type rec anyArg = AnyArg(Arg.arg<_>): anyArg
type rec anyEnumValue = AnyEnumValue(enumValue<_>): anyEnumValue

/* Extracts all types contained in a single type */
let rec collectTypes:
  type ctx src. (
    ~visited: MutableSet.String.t=?,
    ~result: array<anyTyp>=?,
    typ<ctx, src>,
  ) => (array<anyTyp>, MutableSet.String.t) =

  // TODO: add fields arguments

  // add argument types here
  // arg_list_types(f.args)
  (~visited=MutableSet.String.make(), ~result=[], typ) => {
    switch typ {
    | List(typ) => collectTypes(~visited, typ)
    | NonNull(typ) => collectTypes(~visited, typ)
    | Scalar({name}) as scalar =>
      if !MutableSet.String.has(visited, name) {
        MutableSet.String.add(visited, name)
        result->Js.Array2.push(AnyTyp(scalar))->ignore
      }
      (result, visited)
    | Enum({name}) as enum =>
      if !MutableSet.String.has(visited, name) {
        MutableSet.String.add(visited, name)
        result->Js.Array2.push(AnyTyp(enum))->ignore
      }
      (result, visited)
    | Object({name, fields}) as obj =>
      if !MutableSet.String.has(visited, name) {
        MutableSet.String.add(visited, name)
        result->Js.Array2.push(AnyTyp(obj))->ignore

        let fields = Lazy.force(fields)

        List.forEach(fields, (Field(field)) => {
          collectTypes(~visited, ~result, field.typ)->ignore
        })
      }

      (result, visited)
    | Abstract({name, types}) as abstract =>
      if !MutableSet.String.has(visited, name) {
        MutableSet.String.add(visited, name)
        result->Js.Array2.push(AnyTyp(abstract))->ignore

        Js.Array2.forEach(types, typ => {
          switch typ {
          | AnyTyp(typ) => collectTypes(~visited, ~result, typ)->ignore
          | AnyArgTyp(_) => failwith("Abstracts can't have argument types")
          }
        })
      }
      (result, visited)
    }
  }

and arg_collectTypes:
  type arg. (
    ~visited: MutableSet.String.t=?,
    ~result: array<anyTyp>=?,
    Arg.argTyp<arg>,
  ) => array<anyTyp> =
  (~visited=MutableSet.String.make(), ~result=[], argTyp) => {
    switch argTyp {
    | Arg.List(typ) => arg_collectTypes(~visited, ~result, typ)
    | Arg.NonNull(typ) => arg_collectTypes(~visited, ~result, typ)
    | Arg.Scalar({name}) as scalar =>
      if !MutableSet.String.has(visited, name) {
        MutableSet.String.add(visited, name)
        result->Js.Array2.push(AnyArgTyp(scalar))->ignore
      }
      result
    | Arg.Enum({name}) as enum =>
      if !MutableSet.String.has(visited, name) {
        MutableSet.String.add(visited, name)
        result->Js.Array2.push(AnyArgTyp(enum))->ignore
      }
      result
    | Arg.InputObject({name}) as obj =>
      if !MutableSet.String.has(visited, name) {
        MutableSet.String.add(visited, name)
        result->Js.Array2.push(AnyArgTyp(obj))->ignore

        // add to arg list types
        // arg_list_types(memo', o.fields)
      }
      result
    }
  }

and arg_list_types:
  type arg. (
    ~visited: MutableSet.String.t=?,
    ~result: array<anyTyp>=?,
    Arg.argList<arg>,
  ) => array<anyTyp> =
  (~visited=MutableSet.String.make(), ~result=[], argList) => {
    open Arg

    let typesForAg = arg =>
      switch arg {
      | Arg(a) => arg_collectTypes(~visited, ~result, a.typ)
      | DefaultArg(a) => arg_collectTypes(~visited, ~result, a.typ)
      }

    switch argList {
    | Empty => []
    | Arg1(arg) => typesForAg(arg)
    | Arg2(arg1, arg2) => Array.concat(typesForAg(arg1), typesForAg(arg2))
    | Arg3(arg1, arg2, arg3) =>
      Array.concatMany([typesForAg(arg1), typesForAg(arg2), typesForAg(arg3)])
    }
  }

let args_to_list:
  type a. Arg.argList<a> => array<anyArg> =
  args => {
    open Arg
    switch args {
    | Empty => []
    | Arg1(a) => [AnyArg(a)]
    | Arg2(a1, a2) => [AnyArg(a1), AnyArg(a2)]
    | Arg3(a1, a2, a3) => [AnyArg(a1), AnyArg(a2), AnyArg(a3)]
    }
  }

let __type_kind = Enum({
  name: "__TypeKind",
  description: None,
  values: list{
    {name: "SCALAR", description: None, deprecated: NotDeprecated, value: #Scalar},
    {name: "OBJECT", description: None, deprecated: NotDeprecated, value: #Object},
    {name: "INTERFACE", description: None, deprecated: NotDeprecated, value: #Interface},
    {name: "UNION", description: None, deprecated: NotDeprecated, value: #Union},
    {name: "ENUM", description: None, deprecated: NotDeprecated, value: #Enum},
    {
      name: "INPUT_OBJECT",
      description: None,
      deprecated: NotDeprecated,
      value: #InputObject,
    },
    {name: "LIST", description: None, deprecated: NotDeprecated, value: #List},
    {name: "NON_NULL", description: None, deprecated: NotDeprecated, value: #NonNull},
  },
})

let no_abstracts = ref(list{})

let __enumValue = Object({
  name: "__EnumValue",
  description: None,
  abstracts: no_abstracts,
  fields: lazy list{
    Field({
      name: "name",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(string),
      args: Empty,
      resolve: (_, AnyEnumValue(enum_value), _) => Promise.resolve(enum_value.name),
    }),
    Field({
      name: "description",
      description: None,
      deprecated: NotDeprecated,
      typ: string,
      args: Empty,
      resolve: (_, AnyEnumValue(enum_value), _) => Promise.resolve(enum_value.description),
    }),
    Field({
      name: "isDeprecated",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(boolean),
      args: Empty,
      resolve: (_, AnyEnumValue(enum_value), _) =>
        Promise.resolve(enum_value.deprecated != NotDeprecated),
    }),
    Field({
      name: "deprecationReason",
      description: None,
      deprecated: NotDeprecated,
      typ: string,
      args: Empty,
      resolve: (_, AnyEnumValue(enum_value), _) =>
        switch enum_value.deprecated {
        | Deprecated(reason) => reason
        | NotDeprecated => None
        } |> Promise.resolve,
    }),
  },
})

let rec __input_value = Object({
  name: "__InputValue",
  description: None,
  abstracts: no_abstracts,
  fields: lazy list{
    Field({
      name: "name",
      typ: NonNull(string),
      args: Empty,
      deprecated: NotDeprecated,
      description: None,
      resolve: (_, AnyArg(arg), _) =>
        switch arg {
        | Arg.DefaultArg(a) => a.name
        | Arg.Arg(a) => a.name
        } |> Promise.resolve,
    }),
    Field({
      name: "description",
      typ: string,
      args: Empty,
      deprecated: NotDeprecated,
      description: None,
      resolve: (_, AnyArg(arg), _) =>
        switch arg {
        | Arg.DefaultArg(a) => a.description
        | Arg.Arg(a) => a.description
        } |> Promise.resolve,
    }),
    Field({
      name: "type",
      typ: NonNull(__type),
      args: Empty,
      deprecated: NotDeprecated,
      description: None,
      resolve: (_, AnyArg(arg), _) =>
        switch arg {
        | Arg.DefaultArg(a) => AnyArgTyp(a.typ)
        | Arg.Arg(a) => AnyArgTyp(a.typ)
        } |> Promise.resolve,
    }),
    Field({
      name: "defaultValue",
      typ: string,
      args: Empty,
      deprecated: NotDeprecated,
      description: None,
      resolve: (_, AnyArg(_), _) => Promise.resolve(None),
    }),
  },
})

and __type = Object({
  name: "__Type",
  description: None,
  abstracts: no_abstracts,
  fields: lazy list{
    Field({
      name: "kind",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(__type_kind),
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyTyp(Object(_)) => #Object
        | AnyTyp(Abstract({kind: #Union, _})) => #Union
        | AnyTyp(Abstract({kind: #Interface(_), _})) => #Interface
        | AnyTyp(List(_)) => #List
        | AnyTyp(Scalar(_)) => #Scalar
        | AnyTyp(Enum(_)) => #Enum
        | AnyTyp(NonNull(_)) => #NonNull
        | AnyArgTyp(Arg.InputObject(_)) => #InputObject
        | AnyArgTyp(Arg.List(_)) => #List
        | AnyArgTyp(Arg.Scalar(_)) => #Scalar
        | AnyArgTyp(Arg.Enum(_)) => #Enum
        | AnyArgTyp(Arg.NonNull(_)) => #NonNull
        } |> Promise.resolve,
    }),
    Field({
      name: "ofType",
      description: None,
      deprecated: NotDeprecated,
      typ: __type,
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyTyp(NonNull(typ)) => Some(AnyTyp(typ))
        | AnyTyp(List(typ)) => Some(AnyTyp(typ))
        | AnyArgTyp(Arg.NonNull(typ)) => Some(AnyArgTyp(typ))
        | AnyArgTyp(Arg.List(typ)) => Some(AnyArgTyp(typ))
        | _ => None
        } |> Promise.resolve,
    }),
    Field({
      name: "name",
      description: None,
      deprecated: NotDeprecated,
      typ: string,
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyTyp(Object(o)) => Some(o.name)
        | AnyTyp(Scalar(s)) => Some(s.name)
        | AnyTyp(Enum(e)) => Some(e.name)
        | AnyTyp(Abstract(a)) => Some(a.name)
        | AnyArgTyp(Arg.InputObject(o)) => Some(o.name)
        | AnyArgTyp(Arg.Scalar(s)) => Some(s.name)
        | AnyArgTyp(Arg.Enum(e)) => Some(e.name)
        | _ => None
        } |> Promise.resolve,
    }),
    Field({
      name: "description",
      description: None,
      deprecated: NotDeprecated,
      typ: string,
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyTyp(Object(o)) => o.description
        | AnyTyp(Scalar(s)) => s.description
        | AnyTyp(Enum(e)) => e.description
        | AnyTyp(Abstract(a)) => a.description
        | AnyArgTyp(Arg.InputObject(o)) => o.description
        | AnyArgTyp(Arg.Scalar(s)) => s.description
        | AnyArgTyp(Arg.Enum(e)) => e.description
        | _ => None
        } |> Promise.resolve,
    }),
    Field({
      name: "fields",
      description: None,
      deprecated: NotDeprecated,
      typ: List(NonNull(__field)),
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyTyp(Object(o)) => Some(List.map(Lazy.force(o.fields), f => AnyField(f))->List.toArray)
        | AnyTyp(Abstract({kind: #Interface(fields), _})) =>
          Some(Array.map(Lazy.force(fields), (AbstractField(f)) => AnyField(f)))
        | AnyArgTyp(Arg.InputObject(o)) =>
          let arg_list = args_to_list(o.fields)
          Some(Array.map(arg_list, (AnyArg(f)) => AnyArgField(f)))
        | _ => None
        } |> Promise.resolve,
    }),
    Field({
      name: "interfaces",
      description: None,
      deprecated: NotDeprecated,
      typ: List(NonNull(__type)),
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyTyp(Object(o)) =>
          let interfaces = List.keep(o.abstracts.contents, x =>
            switch x {
            | {kind: #Interface(_), _} => true
            | _ => false
            }
          )
          Some(List.map(interfaces, i => AnyTyp(Abstract(i))) |> List.toArray)
        | _ => None
        } |> Promise.resolve,
    }),
    Field({
      name: "possibleTypes",
      description: None,
      deprecated: NotDeprecated,
      typ: List(NonNull(__type)),
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyTyp(Abstract(a)) => Some(a.types)
        | _ => None
        } |> Promise.resolve,
    }),
    Field({
      name: "inputFields",
      description: None,
      deprecated: NotDeprecated,
      typ: List(NonNull(__input_value)),
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyArgTyp(Arg.InputObject(o)) => Some(args_to_list(o.fields))
        | _ => None
        } |> Js.Promise.resolve,
    }),
    Field({
      name: "enumValues",
      description: None,
      deprecated: NotDeprecated,
      typ: List(NonNull(__enumValue)),
      args: Empty,
      resolve: (_, t, _) =>
        switch t {
        | AnyTyp(Enum(e)) => Some(List.map(e.values, x => AnyEnumValue(x))->List.toArray)
        | AnyArgTyp(Arg.Enum(e)) => Some(List.map(e.values, x => AnyEnumValue(x))->List.toArray)
        | _ => None
        } |> Promise.resolve,
    }),
  },
})

and __field = Object({
  name: "__Field",
  description: None,
  abstracts: no_abstracts,
  fields: lazy list{
    Field({
      name: "name",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(string),
      args: Empty,
      resolve: (_, f, _) =>
        switch f {
        | AnyField(Field(f)) => f.name
        | AnyArgField(Arg.Arg(a)) => a.name
        | AnyArgField(Arg.DefaultArg(a)) => a.name
        } |> Promise.resolve,
    }),
    Field({
      name: "description",
      description: None,
      deprecated: NotDeprecated,
      typ: string,
      args: Empty,
      resolve: (_, f, _) =>
        switch f {
        | AnyField(Field(f)) => f.description
        | AnyArgField(Arg.Arg(a)) => a.description
        | AnyArgField(Arg.DefaultArg(a)) => a.description
        } |> Promise.resolve,
    }),
    Field({
      name: "args",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(List(NonNull(__input_value))),
      args: Empty,
      resolve: (_, f, _) =>
        switch f {
        | AnyField(Field(f)) => args_to_list(f.args)
        | AnyArgField(_) => []
        } |> Promise.resolve,
    }),
    Field({
      name: "type",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(__type),
      args: Empty,
      resolve: (_, f, _) =>
        switch f {
        | AnyField(Field(f)) => AnyTyp(f.typ)
        | AnyArgField(Arg.Arg(a)) => AnyArgTyp(a.typ)
        | AnyArgField(Arg.DefaultArg(a)) => AnyArgTyp(a.typ)
        } |> Promise.resolve,
    }),
    Field({
      name: "isDeprecated",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(boolean),
      args: Empty,
      resolve: (_, f, _) =>
        switch f {
        | AnyField(Field({deprecated: Deprecated(_), _})) => true
        | _ => false
        } |> Promise.resolve,
    }),
    Field({
      name: "deprecationReason",
      description: None,
      deprecated: NotDeprecated,
      typ: string,
      args: Empty,
      resolve: (_, f, _) =>
        switch f {
        | AnyField(Field({deprecated: Deprecated(reason), _})) => reason
        | _ => None
        } |> Promise.resolve,
    }),
  },
})

let __directiveLocation = Enum({
  name: "__DirectiveLocation",
  description: None,
  values: list{
    {name: "QUERY", description: None, deprecated: NotDeprecated, value: #Query},
    {name: "MUTATION", description: None, deprecated: NotDeprecated, value: #Mutation},
    {
      name: "SUBSCRIPTION",
      description: None,
      deprecated: NotDeprecated,
      value: #Subscription,
    },
    {name: "FIELD", description: None, deprecated: NotDeprecated, value: #Field},
    {
      name: "FragmentDefinition",
      description: None,
      deprecated: NotDeprecated,
      value: #FragmentDefinition,
    },
    {
      name: "FragmentSpread",
      description: None,
      deprecated: NotDeprecated,
      value: #FragmentSpread,
    },
    {
      name: "InlineFragment",
      description: None,
      deprecated: NotDeprecated,
      value: #InlineFragment,
    },
    {
      name: "VariableDefinition",
      description: None,
      deprecated: NotDeprecated,
      value: #VariableDefinition,
    },
  },
})

let __directive = Object({
  name: "__Directive",
  description: None,
  abstracts: no_abstracts,
  fields: lazy list{
    Field({
      name: "name",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(string),
      args: Empty,
      resolve: (_, Directive(d), _) => Promise.resolve(d.name),
    }),
    Field({
      name: "description",
      description: None,
      deprecated: NotDeprecated,
      typ: string,
      args: Empty,
      resolve: (_, Directive(d), _) => Promise.resolve(d.description),
    }),
    Field({
      name: "locations",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(List(NonNull(__directiveLocation))),
      args: Empty,
      resolve: (_, Directive(d), _) => Promise.resolve(d.locations->List.toArray),
    }),
    Field({
      name: "args",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(List(NonNull(__input_value))),
      args: Empty,
      resolve: (_, Directive(d), _) => Promise.resolve(args_to_list(d.args)),
    }),
  },
})

let __schema = Object({
  name: "__Schema",
  description: None,
  abstracts: no_abstracts,
  fields: lazy list{
    Field({
      name: "types",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(List(NonNull(__type))),
      args: Empty,
      resolve: (_, s, _) => {
        let (types, visited) = collectTypes(~result=[], Object(s.query))

        switch s.mutation {
        | Some(mutation) =>
          let (types, _visited) = collectTypes(~visited, ~result=types, Object(mutation))
          Promise.resolve(types)
        | None => Promise.resolve(types)
        }
      },
    }),
    Field({
      name: "queryType",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(__type),
      args: Empty,
      resolve: (_, s, _) => Promise.resolve(AnyTyp(Object(s.query))),
    }),
    Field({
      name: "mutationType",
      description: None,
      deprecated: NotDeprecated,
      typ: __type,
      args: Empty,
      resolve: (_, s, _) => Promise.resolve(Option.map(s.mutation, mut => AnyTyp(Object(mut)))),
    }),
    Field({
      name: "subscriptionType",
      description: None,
      deprecated: NotDeprecated,
      typ: __type,
      args: Empty,
      resolve: (_, _, _) => Promise.resolve(None),
    }),
    Field({
      name: "directives",
      description: None,
      deprecated: NotDeprecated,
      typ: NonNull(List(NonNull(__directive))),
      args: Empty,
      resolve: (_, _, _) => Promise.resolve([]),
    }),
  },
})

let addSchemaField = schema => {
  ...schema,
  query: {
    ...schema.query,
    fields: lazy {
      list{
        Field({
          name: "__schema",
          typ: NonNull(__schema),
          args: Empty,
          description: None,
          deprecated: NotDeprecated,
          resolve: (_, _, _) => Promise.resolve(schema),
        }),
        ...Lazy.force(schema.query.fields),
      }
    },
  },
}

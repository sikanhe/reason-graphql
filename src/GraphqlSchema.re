open GraphqlLanguage;

module Result = Belt.Result;
module Option = Belt.Option;

module List = {
  include Belt.List;

  module Result = {
    include Belt.Result;

    let rec join = (~memo=[]) =>
      fun
      | [] => Ok(Belt.List.reverse(memo))
      | [Error(_) as err, ..._] => err
      | [Ok(x), ...xs] => join(~memo=[x, ...memo], xs);

    let all = (list, f) => Belt.List.map(list, f) |> join;
  };
};

module StringMap = {
  include Belt.Map.String;
  exception MissingKey(string);

  let getExn = (map, key) =>
    try (getExn(map, key)) {
    | Not_found => raise(MissingKey(key))
    };
};

module StringSet = Set.Make(String);

type variableList = list((string, Ast.constValue));
type variableMap = StringMap.t(Ast.constValue);
type fragmentMap = StringMap.t(Ast.fragmentDefinition);

module type IO = {
  type t(+'a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
};

let id: 'a. 'a => 'a = x => x;

module Make = (Io: IO) => {
  open Result;

  module Io = {
    include Io;

    let ok = x => return(Ok(x));
    let error = x => return(Error(x));
    let map = (x, f) => bind(x, x' => return(f(x')));

    let rec all =
      fun
      | [] => return([])
      | [x, ...xs] => bind(all(xs), xs' => map(x, x' => [x', ...xs']));

    module Result = {
      let bind = (x, f) =>
        bind(
          x,
          fun
          | Ok(x') => f(x')
          | Error(_) as err => return(err),
        );

      let mapError = (x, f) =>
        map(
          x,
          fun
          | Ok(_) as ok => ok
          | Error(err) => Error(f(err)),
        );

      let map = (x, f) =>
        map(
          x,
          fun
          | Ok(x') => Ok(f(x'))
          | Error(_) as err => err,
        );

      let let_ = bind;
    };

    let rec mapSerial = (~memo=[], list, f) =>
      switch (list) {
      | [] => Io.return(List.reverse(memo))
      | [x, ...xs] => bind(f(x), x' => mapSerial(~memo=[x', ...memo], xs, f))
      };

    let mapParalell = (list, f) => list->List.map(f)->all;

    let let_ = bind;
  };

  type deprecation =
    | NotDeprecated
    | Deprecated(option(string));

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
    and argTyp(_) =
      | Scalar(scalar('a)): argTyp(option('a))
      | Enum(enum('a)): argTyp(option('a))
      | InputObject(inputObject('a, 'b)): argTyp(option('a))
      | List(argTyp('a)): argTyp(option(list('a)))
      | NonNull(argTyp(option('a))): argTyp('a)
    and scalar('a) = {
      name: string,
      description: option(string),
      parse: Ast.constValue => Result.t('a, string),
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
      typ: argTyp('a),
    }
    and argumentWithDefault('a) = {
      name: string,
      description: option(string),
      typ: argTyp(option('a)),
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
          | _ => Error("Invalid string")
          },
      });

    let int =
      Scalar({
        name: "Int",
        description: None,
        parse: input =>
          switch (input) {
          | `Int(int) => Ok(int)
          | _ => Error("Invalid integer")
          },
      });

    let float =
      Scalar({
        name: "Float",
        description: None,
        parse: input =>
          switch (input) {
          | `Float(float) => Ok(float)
          | _ => Error("Invalid float")
          },
      });

    let boolean =
      Scalar({
        name: "Boolean",
        description: None,
        parse: input =>
          switch (input) {
          | `Boolean(bool) => Ok(bool)
          | _ => Error("Invalid boolean")
          },
      });

    let list = a => List(a);
    let nonnull = a => NonNull(a);
  };

  type typ(_, _) =
    | Scalar(scalar('src)): typ('ctx, option('src))
    | Enum(enum('src)): typ('ctx, option('src))
    | List(typ('ctx, 'src)): typ('ctx, option(list('src)))
    | Object(obj('ctx, 'src)): typ('ctx, option('src))
    | Abstract(abstract): typ('ctx, option(abstractValue('ctx, 'a)))
    | NonNull(typ('ctx, option('src))): typ('ctx, 'src)
  and scalar('src) = {
    name: string,
    description: option(string),
    serialize: 'src => Ast.constValue,
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
    lift: 'a => Io.t(Result.t('out, string)),
  }
  and anyTyp =
    | AnyTyp(typ(_, _)): anyTyp
    | AnyArgTyp(Arg.argTyp(_)): anyTyp
  and abstract = {
    name: string,
    description: option(string),
    mutable types: list(anyTyp),
    kind: [ | `Union | `Interface(Lazy.t(list(abstractField)))],
  }
  and abstractField =
    | AbstractField(field(_, _)): abstractField
  and abstractValue('ctx, 'a) =
    | AbstractValue((typ('ctx, option('src)), 'src)): abstractValue('ctx, 'a);

  type abstractType('ctx, 'a) = typ('ctx, option(abstractValue('ctx, 'a)));

  type directiveLocation = [
    | `Query
    | `Mutation
    | `Subscription
    | `Field
    | `FragmentDefinition
    | `FragmentSpread
    | `InlineFragment
    | `VariableDefinition
  ];

  type directiveInfo('args) = {
    name: string,
    description: option(string),
    locations: list(directiveLocation),
    args: Arg.arglist([ | `Skip | `Include], 'args),
    resolve: 'args,
  };

  type directive =
    | Directive(directiveInfo('args)): directive;

  let skipDirective =
    Directive({
      name: "skip",
      description:
        Some(
          "Directs the executor to skip this field or fragment when the `if` argument is true.",
        ),
      locations: [`Field, `FragmentSpread, `InlineFragment],
      args: Arg.[arg("if", nonnull(boolean), ~description="Skipped when true.")],
      resolve:
        fun
        | true => `Skip
        | false => `Include,
    });

  let includeDirective =
    Directive({
      name: "include",
      description:
        Some(
          "Directs the executor to include this field or fragment only when the `if` argument is true.",
        ),
      locations: [`Field, `FragmentSpread, `InlineFragment],
      args: Arg.[arg("if", nonnull(boolean), ~description="Included when true.")],
      resolve:
        fun
        | true => `Include
        | false => `Skip,
    });

  type schema('ctx) = {
    query: obj('ctx, unit),
    mutation: option(obj('ctx, unit)),
  };

  type combinedEnum('ctx, 'a) = {
    argTyp: Arg.argTyp('a),
    fieldType: typ('ctx, 'a),
  };

  let makeEnum = (name, ~description=?, values) => {
    argTyp: Arg.Enum({name, description, values}),
    fieldType: Enum({name, description, values}),
  };

  let enumValue = (~description=?, ~deprecated=NotDeprecated, ~value, name) => {
    name,
    description,
    deprecated,
    value,
  };

  let obj = (~description=?, ~implements: ref(list(abstract))=ref([]), ~fields, name) => {
    let rec self =
      Object({name, description, fields: lazy (fields(self)), abstracts: implements});
    self;
  };

  let field = (~description=?, ~deprecated=NotDeprecated, ~args, ~resolve, name, typ) =>
    Field({name, typ, resolve, deprecated, description, args, lift: Io.ok});

  let async_field = (~description=?, ~deprecated=NotDeprecated, ~args, ~resolve, name, typ) =>
    Field({name, typ, resolve, deprecated, description, args, lift: id});

  let abstractField = (~description=?, ~deprecated=NotDeprecated, ~args, name, typ) =>
    AbstractField(
      Field({lift: Io.ok, name, description, deprecated, typ, args, resolve: Obj.magic()}),
    );

  let union = (~description=?, name) => Abstract({name, description, types: [], kind: `Union});

  let interface = (~description=?, ~fields, name) => {
    let rec t = Abstract({name, description, types: [], kind: `Interface(lazy (fields(t)))});
    t;
  };

  let addType = (abstractType, typ) => {
    switch (abstractType, typ) {
    | (Abstract(a), Object(o)) =>
      a.types = [AnyTyp(typ), ...a.types];
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

  let create = (~mutation=?, query) => {query, mutation};

  /* Built in scalars */
  let string: 'ctx. typ('ctx, option(string)) =
    Scalar({name: "String", description: None, serialize: str => `String(str)});
  let int: 'ctx. typ('ctx, option(int)) =
    Scalar({name: "Int", description: None, serialize: int => `Int(int)});
  let float: 'ctx. typ('ctx, option(float)) =
    Scalar({name: "Float", description: None, serialize: float => `Float(float)});
  let boolean: 'ctx. typ('ctx, option(bool)) =
    Scalar({name: "Boolean", description: None, serialize: bool => `Boolean(bool)});
  let list = typ => List(typ);
  let nonnull = typ => NonNull(typ);

  module Introspection = {
    /* anyTyp, anyField and anyArg hide type parameters to avoid scope escaping errors */
    type anyField =
      | AnyField(field(_, _)): anyField
      | AnyArgField(Arg.arg(_)): anyField;
    type anyArg =
      | AnyArg(Arg.arg(_)): anyArg;
    type anyEnumValue =
      | AnyEnumValue(enumValue(_)): anyEnumValue;

    let unlessVisited = ((result, visited), name, f) =>
      if (StringSet.mem(name, visited)) {
        (result, visited);
      } else {
        f((result, visited));
      };

    /* Extracts all types contained in a single type */


    let rec types:
      type ctx src.
        (~memo: (list(anyTyp), StringSet.t)=?, typ(ctx, src)) => (list(anyTyp), StringSet.t) =
      (~memo=([], StringSet.empty), typ) =>
        switch (typ) {
        | List(typ) => types(~memo, typ)
        | NonNull(typ) => types(~memo, typ)
        | Scalar(s) as scalar =>
          unlessVisited(memo, s.name, ((result, visited)) =>
            ([AnyTyp(scalar), ...result], StringSet.add(s.name, visited))
          )
        | Enum(e) as enum =>
          unlessVisited(memo, e.name, ((result, visited)) =>
            ([AnyTyp(enum), ...result], StringSet.add(e.name, visited))
          )
        | Object(o) as obj =>
          unlessVisited(
            memo,
            o.name,
            ((result, visited)) => {
              let result' = [AnyTyp(obj), ...result];
              let visited' = StringSet.add(o.name, visited);
              let reducer = (memo, Field(f)) => {
                let memo' = types(~memo, f.typ);
                arg_list_types(memo', f.args);
              };

              List.reduceReverse(Lazy.force(o.fields), (result', visited'), reducer);
            },
          )
        | Abstract(a) as abstract =>
          unlessVisited(
            memo,
            a.name,
            ((result, visited)) => {
              let result' = [AnyTyp(abstract), ...result];
              let visited' = StringSet.add(a.name, visited);
              List.reduceReverse(a.types, (result', visited'), (memo, typ) =>
                switch (typ) {
                | AnyTyp(typ) => types(~memo, typ)
                | AnyArgTyp(_) => failwith("Abstracts can't have argument types")
                }
              );
            },
          )
        }

    and arg_types:
      type a. ((list(anyTyp), StringSet.t), Arg.argTyp(a)) => (list(anyTyp), StringSet.t) =
      (memo, argtyp) =>
        switch (argtyp) {
        | Arg.List(typ) => arg_types(memo, typ)
        | Arg.NonNull(typ) => arg_types(memo, typ)
        | Arg.Scalar(s) as scalar =>
          unlessVisited(memo, s.name, ((result, visited)) =>
            ([AnyArgTyp(scalar), ...result], StringSet.add(s.name, visited))
          )
        | Arg.Enum(e) as enum =>
          unlessVisited(memo, e.name, ((result, visited)) =>
            ([AnyArgTyp(enum), ...result], StringSet.add(e.name, visited))
          )
        | Arg.InputObject(o) as obj =>
          unlessVisited(
            memo,
            o.name,
            ((result, visited)) => {
              let memo' = ([AnyArgTyp(obj), ...result], StringSet.add(o.name, visited));
              arg_list_types(memo', o.fields);
            },
          )
        }

    and arg_list_types:
      type a b.
        ((list(anyTyp), StringSet.t), Arg.arglist(a, b)) => (list(anyTyp), StringSet.t) =
      (memo, arglist) =>
        Arg.(
          switch (arglist) {
          | [] => memo
          | [arg, ...args] =>
            let memo' =
              switch (arg) {
              | Arg(a) => arg_types(memo, a.typ)
              | DefaultArg(a) => arg_types(memo, a.typ)
              };
            arg_list_types(memo', args);
          }
        );

    let rec args_to_list: type a b. (~memo: list(anyArg)=?, Arg.arglist(a, b)) => list(anyArg) =
      (~memo=[], arglist) =>
        Arg.(
          switch (arglist) {
          | [] => memo
          | [arg, ...args] =>
            let arg = AnyArg(arg);
            let memo': list(anyArg) = [arg, ...memo];
            args_to_list(~memo=memo', args);
          }
        );

    let no_abstracts = ref([]);

    let __type_kind =
      Enum({
        name: "__TypeKind",
        description: None,
        values: [
          {name: "SCALAR", description: None, deprecated: NotDeprecated, value: `Scalar},
          {name: "OBJECT", description: None, deprecated: NotDeprecated, value: `Object},
          {name: "INTERFACE", description: None, deprecated: NotDeprecated, value: `Interface},
          {name: "UNION", description: None, deprecated: NotDeprecated, value: `Union},
          {name: "ENUM", description: None, deprecated: NotDeprecated, value: `Enum},
          {
            name: "INPUT_OBJECT",
            description: None,
            deprecated: NotDeprecated,
            value: `InputObject,
          },
          {name: "LIST", description: None, deprecated: NotDeprecated, value: `List},
          {name: "NON_NULL", description: None, deprecated: NotDeprecated, value: `NonNull},
        ],
      });

    let __enumValue: 'ctx. typ('ctx, option(anyEnumValue)) =
      Object({
        name: "__EnumValue",
        description: None,
        abstracts: no_abstracts,
        fields:
          lazy [
            Field({
              name: "name",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(string),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, AnyEnumValue(enum_value)) => enum_value.name,
            }),
            Field({
              name: "description",
              description: None,
              deprecated: NotDeprecated,
              typ: string,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, AnyEnumValue(enum_value)) => {
                enum_value.description;
              },
            }),
            Field({
              name: "isDeprecated",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(boolean),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, AnyEnumValue(enum_value)) => enum_value.deprecated != NotDeprecated,
            }),
            Field({
              name: "deprecationReason",
              description: None,
              deprecated: NotDeprecated,
              typ: string,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, AnyEnumValue(enum_value)) =>
                switch (enum_value.deprecated) {
                | Deprecated(reason) => reason
                | NotDeprecated => None
                },
            }),
          ],
      });

    let rec __input_value: 'ctx. typ('ctx, option(anyArg)) =
      Object({
        name: "__InputValue",
        description: None,
        abstracts: no_abstracts,
        fields:
          lazy [
            Field({
              name: "name",
              typ: NonNull(string),
              args: Arg.[],
              deprecated: NotDeprecated,
              description: None,
              lift: Io.ok,
              resolve: (_, AnyArg(arg)) =>
                switch (arg) {
                | Arg.DefaultArg(a) => a.name
                | Arg.Arg(a) => a.name
                },
            }),
            Field({
              name: "description",
              typ: string,
              args: Arg.[],
              deprecated: NotDeprecated,
              description: None,
              lift: Io.ok,
              resolve: (_, AnyArg(arg)) =>
                switch (arg) {
                | Arg.DefaultArg(a) => a.description
                | Arg.Arg(a) => a.description
                },
            }),
            Field({
              name: "type",
              typ: NonNull(__type),
              args: Arg.[],
              deprecated: NotDeprecated,
              description: None,
              lift: Io.ok,
              resolve: (_, AnyArg(arg)) =>
                switch (arg) {
                | Arg.DefaultArg(a) => AnyArgTyp(a.typ)
                | Arg.Arg(a) => AnyArgTyp(a.typ)
                },
            }),
          ],
      })
    and __type: 'ctx. typ('ctx, option(anyTyp)) =
      Object({
        name: "__Type",
        description: None,
        abstracts: no_abstracts,
        fields:
          lazy [
            Field({
              name: "kind",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(__type_kind),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyTyp(Object(_)) => `Object
                | AnyTyp(Abstract({kind: `Union, _})) => `Union
                | AnyTyp(Abstract({kind: `Interface(_), _})) => `Interface
                | AnyTyp(List(_)) => `List
                | AnyTyp(Scalar(_)) => `Scalar
                | AnyTyp(Enum(_)) => `Enum
                | AnyTyp(NonNull(_)) => `NonNull
                | AnyArgTyp(Arg.InputObject(_)) => `InputObject
                | AnyArgTyp(Arg.List(_)) => `List
                | AnyArgTyp(Arg.Scalar(_)) => `Scalar
                | AnyArgTyp(Arg.Enum(_)) => `Enum
                | AnyArgTyp(Arg.NonNull(_)) => `NonNull
                },
            }),
            Field({
              name: "ofType",
              description: None,
              deprecated: NotDeprecated,
              typ: __type,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyTyp(NonNull(typ)) => Some(AnyTyp(typ))
                | AnyTyp(List(typ)) => Some(AnyTyp(typ))
                | AnyArgTyp(Arg.NonNull(typ)) => Some(AnyArgTyp(typ))
                | AnyArgTyp(Arg.List(typ)) => Some(AnyArgTyp(typ))
                | _ => None
                },
            }),
            Field({
              name: "name",
              description: None,
              deprecated: NotDeprecated,
              typ: string,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyTyp(Object(o)) => Some(o.name)
                | AnyTyp(Scalar(s)) => Some(s.name)
                | AnyTyp(Enum(e)) => Some(e.name)
                | AnyTyp(Abstract(a)) => Some(a.name)
                | AnyArgTyp(Arg.InputObject(o)) => Some(o.name)
                | AnyArgTyp(Arg.Scalar(s)) => Some(s.name)
                | AnyArgTyp(Arg.Enum(e)) => Some(e.name)
                | _ => None
                },
            }),
            Field({
              name: "description",
              description: None,
              deprecated: NotDeprecated,
              typ: string,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyTyp(Object(o)) => o.description
                | AnyTyp(Scalar(s)) => s.description
                | AnyTyp(Enum(e)) => e.description
                | AnyTyp(Abstract(a)) => a.description
                | AnyArgTyp(Arg.InputObject(o)) => o.description
                | AnyArgTyp(Arg.Scalar(s)) => s.description
                | AnyArgTyp(Arg.Enum(e)) => e.description
                | _ => None
                },
            }),
            Field({
              name: "fields",
              description: None,
              deprecated: NotDeprecated,
              typ: List(NonNull(__field)),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyTyp(Object(o)) => Some(List.map(Lazy.force(o.fields), f => AnyField(f)))
                | AnyTyp(Abstract({kind: `Interface(fields), _})) =>
                  Some(List.map(Lazy.force(fields), (AbstractField(f)) => AnyField(f)))
                | AnyArgTyp(Arg.InputObject(o)) =>
                  let arg_list = args_to_list(o.fields);
                  Some(List.map(arg_list, (AnyArg(f)) => AnyArgField(f)));
                | _ => None
                },
            }),
            Field({
              name: "interfaces",
              description: None,
              deprecated: NotDeprecated,
              typ: List(NonNull(__type)),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyTyp(Object(o)) =>
                  let interfaces =
                    List.keep(
                      o.abstracts^,
                      fun
                      | {kind: `Interface(_), _} => true
                      | _ => false,
                    );
                  Some(List.map(interfaces, i => AnyTyp(Abstract(i))));
                | _ => None
                },
            }),
            Field({
              name: "possibleTypes",
              description: None,
              deprecated: NotDeprecated,
              typ: List(NonNull(__type)),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyTyp(Abstract(a)) => Some(a.types)
                | _ => None
                },
            }),
            Field({
              name: "inputFields",
              description: None,
              deprecated: NotDeprecated,
              typ: List(NonNull(__input_value)),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyArgTyp(Arg.InputObject(o)) => Some(args_to_list(o.fields))
                | _ => None
                },
            }),
            Field({
              name: "enumValues",
              description: None,
              deprecated: NotDeprecated,
              typ: List(NonNull(__enumValue)),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, t) =>
                switch (t) {
                | AnyTyp(Enum(e)) => Some(List.map(e.values, x => AnyEnumValue(x)))
                | AnyArgTyp(Arg.Enum(e)) => Some(List.map(e.values, x => AnyEnumValue(x)))
                | _ => None
                },
            }),
          ],
      })
    and __field: type ctx. typ(ctx, option(anyField)) =
      Object({
        name: "__Field",
        description: None,
        abstracts: no_abstracts,
        fields:
          lazy [
            Field({
              name: "name",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(string),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, f) =>
                switch (f) {
                | AnyField(Field(f)) => f.name
                | AnyArgField(Arg.Arg(a)) => a.name
                | AnyArgField(Arg.DefaultArg(a)) => a.name
                },
            }),
            Field({
              name: "description",
              description: None,
              deprecated: NotDeprecated,
              typ: string,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, f) =>
                switch (f) {
                | AnyField(Field(f)) => f.description
                | AnyArgField(Arg.Arg(a)) => a.description
                | AnyArgField(Arg.DefaultArg(a)) => a.description
                },
            }),
            Field({
              name: "args",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(List(NonNull(__input_value))),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, f) =>
                switch (f) {
                | AnyField(Field(f)) => args_to_list(f.args)
                | AnyArgField(_) => []
                },
            }),
            Field({
              name: "type",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(__type),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, f) =>
                switch (f) {
                | AnyField(Field(f)) => AnyTyp(f.typ)
                | AnyArgField(Arg.Arg(a)) => AnyArgTyp(a.typ)
                | AnyArgField(Arg.DefaultArg(a)) => AnyArgTyp(a.typ)
                },
            }),
            Field({
              name: "isDeprecated",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(boolean),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, f) =>
                switch (f) {
                | AnyField(Field({deprecated: Deprecated(_), _})) => true
                | _ => false
                },
            }),
            Field({
              name: "deprecationReason",
              description: None,
              deprecated: NotDeprecated,
              typ: string,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, f) =>
                switch (f) {
                | AnyField(Field({deprecated: Deprecated(reason), _})) => reason
                | _ => None
                },
            }),
          ],
      });

    let __directiveLocation =
      Enum({
        name: "__DirectiveLocation",
        description: None,
        values: [
          {name: "QUERY", description: None, deprecated: NotDeprecated, value: `Query},
          {name: "MUTATION", description: None, deprecated: NotDeprecated, value: `Mutation},
          {
            name: "SUBSCRIPTION",
            description: None,
            deprecated: NotDeprecated,
            value: `Subscription,
          },
          {name: "FIELD", description: None, deprecated: NotDeprecated, value: `Field},
          {
            name: "FragmentDefinition",
            description: None,
            deprecated: NotDeprecated,
            value: `FragmentDefinition,
          },
          {
            name: "FragmentSpread",
            description: None,
            deprecated: NotDeprecated,
            value: `FragmentSpread,
          },
          {
            name: "InlineFragment",
            description: None,
            deprecated: NotDeprecated,
            value: `InlineFragment,
          },
          {
            name: "VariableDefinition",
            description: None,
            deprecated: NotDeprecated,
            value: `VariableDefinition,
          },
        ],
      });

    let __directive =
      Object({
        name: "__Directive",
        description: None,
        abstracts: no_abstracts,
        fields:
          lazy [
            Field({
              name: "name",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(string),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, Directive(d)) => d.name,
            }),
            Field({
              name: "description",
              description: None,
              deprecated: NotDeprecated,
              typ: string,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, Directive(d)) => d.description,
            }),
            Field({
              name: "locations",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(List(NonNull(__directiveLocation))),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, Directive(d)) => d.locations,
            }),
            Field({
              name: "args",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(List(NonNull(__input_value))),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, Directive(d)) => args_to_list(d.args),
            }),
          ],
      });

    let __schema: 'ctx. typ('ctx, option(schema('ctx))) =
      Object({
        name: "__Schema",
        description: None,
        abstracts: no_abstracts,
        fields:
          lazy [
            Field({
              name: "types",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(List(NonNull(__type))),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, s) => {
                let (types, _) =
                  List.reduceReverse(
                    [
                      Some(s.query),
                      s.mutation,
                      // Option.map(s.subscription, ~f=obj_of_subscription_obj),
                    ],
                    ([], StringSet.empty),
                    (memo, op) =>
                    switch (op) {
                    | None => memo
                    | Some(op) => types(~memo, Object(op))
                    }
                  );
                types;
              },
            }),
            Field({
              name: "queryType",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(__type),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, s) => AnyTyp(Object(s.query)),
            }),
            Field({
              name: "mutationType",
              description: None,
              deprecated: NotDeprecated,
              typ: __type,
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, s) => Option.map(s.mutation, mut => AnyTyp(Object(mut))),
            }),
            // Field({
            //   name: "subscriptionType",
            //   description: None,
            //   deprecated: NotDeprecated,
            //   typ: NonNull(__type),
            //   args: Arg.[],
            //   lift: Io.ok,
            //   resolve: (_, s) =>
            //     Option.map(s.subscription, subs =>
            //       AnyTyp(Object(obj_of_subscription_obj(subs)))
            //     ),
            // }),
            Field({
              name: "directives",
              description: None,
              deprecated: NotDeprecated,
              typ: NonNull(List(NonNull(__directive))),
              args: Arg.[],
              lift: Io.ok,
              resolve: (_, _) => [],
            }),
          ],
      });

    let addSchemaField = schema => {
      {
        ...schema,
        query: {
          ...schema.query,
          fields:
            lazy [
              Field({
                name: "__schema",
                typ: NonNull(__schema),
                args: [],
                description: None,
                deprecated: NotDeprecated,
                lift: Io.ok,
                resolve: (_, _) => schema,
              }),
              ...Lazy.force(schema.query.fields),
            ],
        },
      };
    };
  };

  // Execution

  type executionContext('ctx) = {
    schema: schema('ctx),
    operation: Ast.operationDefinition,
    fragmentMap,
    variableMap,
    ctx: 'ctx,
  };

  type path = list(string);
  type error = (string, path);

  type resolveError = [
    | `ResolveError(error)
    | `ArgumentError(string)
    | `ValidationError(string)
  ];

  type executeError = [
    resolveError
    | `MutationsNotConfigured
    | `SubscriptionsNotConfigured
    | `NoOperationFound
    | `OperationNameRequired
    | `OperationNotFound
  ];

  type executionResult = {data: Ast.constValue};

  module ArgEval = {
    open Arg;

    let rec valueToConstValue: (variableMap, Ast.value) => Ast.constValue =
      variableMap =>
        fun
        | `Null => `Null
        | `Int(_) as i => i
        | `Float(_) as f => f
        | `String(_) as s => s
        | `Boolean(_) as b => b
        | `Enum(_) as e => e
        | `Variable(v) => StringMap.getExn(variableMap, v)
        | `List(xs) => `List(List.map(xs, valueToConstValue(variableMap)))
        | `Map(props) => {
            let props' =
              List.map(props, ((name, value)) =>
                (name, valueToConstValue(variableMap, value))
              );
            `Map(props');
          };

    let rec stringOfConstValue: Ast.constValue => string = (
      fun
      | `Null => "null"
      | `Int(i) => string_of_int(i)
      | `Float(f) => Js.Float.toString(f)
      | `String(s) => Printf.sprintf("\"%s\"", s)
      | `Boolean(b) => string_of_bool(b)
      | `Enum(e) => e
      | `List(l) => {
          let values = List.map(l, i => stringOfConstValue(i));
          Printf.sprintf("[%s]", String.concat(", ", values));
        }
      | `Map(a) => {
          let values =
            List.map(a, ((k, v)) => Printf.sprintf("%s: %s", k, stringOfConstValue(v)));

          Printf.sprintf("{%s}", String.concat(", ", values));
        }:
        Ast.constValue => string
    );

    let rec stringOfArgType: type a. argTyp(a) => string =
      fun
      | Scalar(a) => Printf.sprintf("%s", a.name)
      | InputObject(a) => Printf.sprintf("%s", a.name)
      | Enum(a) => Printf.sprintf("%s", a.name)
      | List(a) => Printf.sprintf("[%s]", stringOfArgType(a))
      | NonNull(a) => Printf.sprintf("%s!", stringOfArgType(a));

    let evalArgError = (~fieldType="field", ~fieldName, ~argName, argTyp, value) => {
      let foundStr =
        switch (value) {
        | Some(v) => Printf.sprintf("found %s", stringOfConstValue(v))
        | None => "but not provided"
        };

      Printf.sprintf(
        "Argument `%s` of type `%s` expected on %s `%s`, %s.",
        argName,
        stringOfArgType(argTyp),
        fieldType,
        fieldName,
        foundStr,
      );
    };


    let rec evalArgList:
      type a b.
        (
          variableMap,
          ~fieldType: string=?,
          ~fieldName: string,
          arglist(a, b),
          list((string, Ast.value)),
          b
        ) =>
        Result.t(a, string) =
      (variableMap, ~fieldType=?, ~fieldName, arglist, key_values, f) =>
        switch (arglist) {
        | [] => Ok(f)
        | [DefaultArg(arg), ...arglist'] =>
          let arglist'' = [
            Arg({name: arg.name, description: arg.description, typ: arg.typ}),
            ...arglist',
          ];

          evalArgList(
            variableMap,
            ~fieldType?,
            ~fieldName,
            arglist'',
            key_values,
            fun
            | None => f(arg.default)
            | Some(v) => f(v),
          );
        | [Arg(arg), ...arglist'] =>
          try (
            {
              let value = List.getAssoc(key_values, arg.name, (==));
              let constValue = Option.map(value, valueToConstValue(variableMap));
              evalArg(
                variableMap,
                ~fieldType?,
                ~fieldName,
                ~argName=arg.name,
                arg.typ,
                constValue,
              )
              ->Result.flatMap(coerced =>
                  evalArgList(
                    variableMap,
                    ~fieldType?,
                    ~fieldName,
                    arglist',
                    key_values,
                    f(coerced),
                  )
                );
            }
          ) {
          | StringMap.MissingKey(key) => Error(Format.sprintf("Missing variable `%s`", key))
          }
        }

    and evalArg:
      type a.
        (
          variableMap,
          ~fieldType: string=?,
          ~fieldName: string,
          ~argName: string,
          argTyp(a),
          option(Ast.constValue)
        ) =>
        Result.t(a, string) =
      (variableMap, ~fieldType=?, ~fieldName, ~argName, typ, value) =>
        switch (typ, value) {
        | (NonNull(_), None) =>
          Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, value))
        | (NonNull(_), Some(`Null)) =>
          Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, value))
        | (Scalar(_), None) => Ok(None)
        | (Scalar(_), Some(`Null)) => Ok(None)
        | (InputObject(_), None) => Ok(None)
        | (InputObject(_), Some(`Null)) => Ok(None)
        | (List(_), None) => Ok(None)
        | (List(_), Some(`Null)) => Ok(None)
        | (Enum(_), None) => Ok(None)
        | (Enum(_), Some(`Null)) => Ok(None)
        | (NonNull(typ), Some(value)) =>
          evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ, Some(value))
          ->Result.flatMap(
              fun
              | Some(value) => Ok(value)
              | None => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, None)),
            )
        | (Scalar(s), Some(value)) =>
          switch (s.parse(value)) {
          | Ok(coerced) => Ok(Some(coerced))
          | Error(_) => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, Some(value)))
          }
        | (InputObject(o), Some(value)) =>
          switch (value) {
          | `Map((props: list((string, Ast.constValue)))) =>
            evalArgList(
              variableMap,
              ~fieldType?,
              ~fieldName,
              o.fields,
              (props :> list((string, Ast.value))),
              o.coerce,
            )
            ->Result.map(coerced => Some(coerced))
          | _ => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, Some(value)))
          }
        | (List(typ), Some(value)) =>
          switch (value) {
          | `List(values) =>
            let optionValues = List.map(values, x => Some(x));
            List.Result.all(
              optionValues,
              evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ),
            )
            ->Result.map(coerced => Some(coerced));
          | value =>
            evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ, Some(value))
            ->Result.map((coerced) => (Some([coerced]): a))
          }
        | (Enum(enum), Some(value)) =>
          switch (value) {
          | `Enum(v)
          | `String(v) =>
            switch (Belt.List.getBy(enum.values, enumValue => enumValue.name == v)) {
            | Some(enumValue) => Ok(Some(enumValue.value))
            | None =>
              Error(
                Printf.sprintf(
                  "Invalid enum value for argument `%s` on field `%s`",
                  argName,
                  fieldName,
                ),
              )
            }
          | _ =>
            Error(
              Printf.sprintf("Expected enum for argument `%s` on field `%s`", argName, fieldName),
            )
          }
        };
  };

  let matchesTypeCondition = (typeCondition: string, obj: obj('ctx, 'src)) =>
    typeCondition == obj.name
    || Belt.List.some(obj.abstracts^, abstract => abstract.name == typeCondition);

  let rec shouldIncludeField = (ctx, directives: list(Ast.directive)) =>
    switch (directives) {
    | [] => Ok(true)
    | [{name: "skip", arguments}, ...rest] =>
      eval_directive(ctx, skipDirective, arguments, rest)
    | [{name: "include", arguments}, ...rest] =>
      eval_directive(ctx, includeDirective, arguments, rest)
    | [{name, _}, ..._] =>
      let err = Format.sprintf("Unknown directive: %s", name);
      Error(err);
    }
  and eval_directive = (ctx, Directive({name, args, resolve, _}), arguments, rest) =>
    ArgEval.evalArgList(
      ctx.variableMap,
      ~fieldType="directive",
      ~fieldName=name,
      args,
      arguments,
      resolve,
    )
    ->Result.flatMap(
        fun
        | `Skip => Ok(false)
        | `Include => shouldIncludeField(ctx, rest),
      );

  let rec collectFields:
    (executionContext('ctx), obj('ctx, 'src), list(Ast.selection)) =>
    Result.t(list(Ast.field), string) =
    (ctx, obj, selectionSet) =>
      selectionSet
      ->List.map(
          fun
          | Ast.Field(field) =>
            shouldIncludeField(ctx, field.directives)
            ->Result.map(shouldInclude => shouldInclude ? [field] : [])
          | Ast.FragmentSpread(fragmentSpread) =>
            switch (StringMap.get(ctx.fragmentMap, fragmentSpread.name)) {
            | Some({typeCondition, selectionSet, directives})
                when matchesTypeCondition(typeCondition, obj) =>
              shouldIncludeField(ctx, directives)
              ->Result.flatMap(shouldInclude =>
                  shouldInclude ? collectFields(ctx, obj, selectionSet) : Ok([])
                )
            | _ => Ok([])
            }
          | Ast.InlineFragment({typeCondition: Some(condition), directives} as inlineFragment)
              when matchesTypeCondition(condition, obj) => {
              shouldIncludeField(ctx, directives)
              ->Result.flatMap(shouldInclude =>
                  shouldInclude ? collectFields(ctx, obj, inlineFragment.selectionSet) : Ok([])
                );
            }
          | Ast.InlineFragment({typeCondition: _}) => Ok([]),
        )
      ->List.Result.join
      ->List.Result.map(Belt.List.flatten);

  let fieldName: Ast.field => string =
    fun
    | {alias: Some(alias)} => alias
    | field => field.name;

  let getObjField = (fieldName: string, obj: obj('ctx, 'src)): option(field('ctx, 'src)) =>
    obj.fields |> Lazy.force |> Belt.List.getBy(_, (Field(field)) => field.name == fieldName);

  let coerceOrNull = (src, f) =>
    switch (src) {
    | Some(src') => f(src')
    | None => Io.ok(`Null)
    };


  let rec resolveValue:
    type ctx src.
      (executionContext(ctx), src, Ast.field, typ(ctx, src)) =>
      Io.t(Result.t(Ast.constValue, [> resolveError])) =
    (executionContext, src, field, typ) =>
      switch (typ) {
      | NonNull(typ') => resolveValue(executionContext, Some(src), field, typ')
      | Scalar(scalar) => coerceOrNull(src, src' => Io.ok(scalar.serialize(src')))
      | Enum(enum) =>
        coerceOrNull(src, src' =>
          switch (Belt.List.getBy(enum.values, enumValue => enumValue.value == src')) {
          | Some(enumValue) => Io.ok(`String(enumValue.name))
          | None => Io.ok(`Null)
          }
        )
      | Object(obj) =>
        coerceOrNull(src, src' =>
          switch (collectFields(executionContext, obj, field.selectionSet)) {
          | Ok(fields) => resolveFields(executionContext, src', obj, fields)
          | Error(e) => Io.error(`ArgumentError(e))
          }
        )
      | List(typ') =>
        coerceOrNull(src, src' =>
          List.map(src', srcItem => resolveValue(executionContext, srcItem, field, typ'))
          ->Io.all
          ->Io.map(List.Result.join)
          ->Io.Result.map(list => `List(list))
        )
      | Abstract(_) =>
        coerceOrNull(
          src,
          src' => {
            let AbstractValue((typ', src')) = src';
            resolveValue(executionContext, Some(src'), field, typ');
          },
        )
      }

  and resolveField:
    type ctx src.
      (executionContext(ctx), src, Ast.field, field(ctx, src)) =>
      Io.t(Result.t((string, Ast.constValue), [> resolveError])) =
    (executionContext, src, field, Field(fieldDef)) => {
      let name = fieldName(field);
      let resolver = fieldDef.resolve(executionContext.ctx, src);

      switch (
        ArgEval.evalArgList(
          executionContext.variableMap,
          ~fieldName=fieldDef.name,
          fieldDef.args,
          field.arguments,
          resolver,
        )
      ) {
      | Ok(unlifted) =>
        let%Io.Result resolved =
          fieldDef.lift(unlifted)->Io.Result.mapError(err => `ResolveError((err, [])));

        let%Io resolvedValue = resolveValue(executionContext, resolved, field, fieldDef.typ);

        Io.return(
          switch (resolvedValue) {
          | Ok(value) => Ok((name, value))
          | Error(`ArgumentError(_) | `ValidationError(_)) as error => error
          | Error(`ResolveError(_)) as error =>
            switch (fieldDef.typ) {
            | NonNull(_) => error
            | _ => Ok((name, `Null))
            }
          },
        );

      | Error(err) => Io.error(`ArgumentError(err))
      };
    }

  and resolveFields:
    type ctx src.
      (executionContext(ctx), src, obj(ctx, src), list(Ast.field)) =>
      Io.t(Result.t(Ast.constValue, [> resolveError])) =
    (executionContext, src, obj, fields) => {
      let mapFields =
        switch (executionContext.operation.operationType) {
        | Query
        | Subscription => Io.mapParalell
        | Mutation => Io.mapSerial(~memo=[])
        };

      mapFields(fields, field =>
        switch (getObjField(field.name, obj)) {
        | Some(objField) => resolveField(executionContext, src, field, objField)
        | None =>
          let err =
            Printf.sprintf("Field '%s' is not defined on type '%s'", field.name, obj.name);
          Io.error(`ValidationError(err));
        }
      )
      ->Io.map(List.Result.join)
      ->Io.Result.map(assocList => `Map(assocList));
    };

  let executeOperation =
      (executionContext: executionContext('ctx), operation: Ast.operationDefinition)
      : Io.t(Result.t(Ast.constValue, [> executeError])) =>
    switch (operation.operationType) {
    | Query =>
      let%Io.Result fields =
        Io.return(
          collectFields(executionContext, executionContext.schema.query, operation.selectionSet),
        )
        ->Io.Result.mapError(e => `ArgumentError(e));

      (
        resolveFields(executionContext, (), executionContext.schema.query, fields):
          Io.t(Result.t(Ast.constValue, resolveError)) :>
          Io.t(Result.t(Ast.constValue, [> executeError]))
      );
    | Mutation =>
      switch (executionContext.schema.mutation) {
      | Some(mutation) =>
        let%Io.Result fields =
          Io.return(collectFields(executionContext, mutation, operation.selectionSet))
          ->Io.Result.mapError(e => `ArgumentError(e));

        (
          resolveFields(executionContext, (), mutation, fields):
            Io.t(Result.t(Ast.constValue, resolveError)) :>
            Io.t(Result.t(Ast.constValue, [> executeError]))
        );
      | None => Io.error(`MutationsNotConfigured)
      }
    | _ => failwith("Subscription Not implemented")
    };

  let collectOperations = (document: Ast.document) =>
    Belt.List.reduceReverse(document.definitions, [], (list, x) =>
      switch (x) {
      | Ast.OperationDefinition(operation) => [operation, ...list]
      | _ => list
      }
    );

  let collectFragments = (document: Ast.document) => {
    Belt.List.reduceReverse(document.definitions, StringMap.empty, fragmentMap =>
      fun
      | Ast.FragmentDefinition(fragment) => StringMap.set(fragmentMap, fragment.name, fragment)
      | _ => fragmentMap
    );
  };

  exception FragmentCycle(list(string));

  let rec validateFragments = fragmentMap =>
    try (
      {
        StringMap.forEach(fragmentMap, (name, _) =>
          validateFragment(fragmentMap, StringSet.empty, name)
        );
        Ok(fragmentMap);
      }
    ) {
    | FragmentCycle(fragmentNames) =>
      let cycle = String.concat(", ", fragmentNames);
      let err = Format.sprintf("Fragment cycle detected: %s", cycle);
      Error(`ValidationError(err));
    }

  and validateFragment = (fragmentMap: fragmentMap, visited, name) => {
    switch (StringMap.get(fragmentMap, name)) {
    | None => ()
    | Some(fragment) when StringSet.mem(fragment.name, visited) =>
      raise(FragmentCycle(StringSet.elements(visited)))
    | Some(fragment) =>
      let visited' = StringSet.add(fragment.name, visited);
      Belt.List.forEach(fragment.selectionSet, validateFragmentSelection(fragmentMap, visited'));
    };
  }

  and validateFragmentSelection = (fragmentMap, visited, selection) =>
    switch (selection) {
    | Field(field) =>
      Belt.List.forEach(field.selectionSet, validateFragmentSelection(fragmentMap, visited))
    | InlineFragment(inlineFragment) =>
      Belt.List.forEach(
        inlineFragment.selectionSet,
        validateFragmentSelection(fragmentMap, visited),
      )
    | FragmentSpread(fragmentSpread) =>
      validateFragment(fragmentMap, visited, fragmentSpread.name)
    };

  let collectAndValidateFragments = doc => {
    let fragments = collectFragments(doc);
    validateFragments(fragments);
  };

  let okResponse = data => {
    `Map([("data", data)]);
  };

  let errorResponse = (~path=?, msg): Ast.constValue => {
    let path' =
      switch (path) {
      | Some(path) => path
      | None => []
      };
    `Map([
      ("data", `Null),
      (
        "errors",
        `List([
          `Map([
            ("message", `String(msg)),
            ("path", `List(List.map(path', s => `String(s)))),
          ]),
        ]),
      ),
    ]);
  };

  let execute =
      (~variables: variableList=[], ~document: Ast.document, schema: schema('ctx), ~ctx: 'ctx) => {
    let execute' = (schema, ctx, document) => {
      let operations = collectOperations(document);
      let%Io.Result fragmentMap = Io.return(collectAndValidateFragments(document));

      let variableMap =
        Belt.List.reduce(variables, StringMap.empty, (map, (name, value)) =>
          StringMap.set(map, name, value)
        );

      let schema' = Introspection.addSchemaField(schema);

      List.map(
        operations,
        operation => {
          let executionContext = {schema: schema', fragmentMap, operation, variableMap, ctx};
          executeOperation(executionContext, operation);
        },
      )
      ->Belt.List.headExn;
    };

    execute'(schema, ctx, document)
    ->Io.map(
        fun
        | Ok(res) => okResponse(res)
        | Error(`NoOperationFound) => errorResponse("No operation found")
        | Error(`OperationNotFound) => errorResponse("Operation not found")
        | Error(`OperationNameRequired) => errorResponse("Operation name required")
        | Error(`SubscriptionsNotConfigured) => errorResponse("Subscriptions not configured")
        | Error(`MutationsNotConfigured) => errorResponse("Mutations not configured")
        | Error(`ValidationError(msg)) => errorResponse(msg)
        | Error(`ArgumentError(msg)) => errorResponse(msg)
        | Error(`ResolveError(msg, path)) => errorResponse(msg, ~path),
      );
  };

  let resultToJson: Io.t(Ast.constValue) => Io.t(Js.Json.t) =
    result => Io.map(result, GraphqlJson.fromConstValue);
};
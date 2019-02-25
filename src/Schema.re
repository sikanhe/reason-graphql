module Result = {
  include Belt.Result;

  let rec join = (~memo=[]) =>
    fun
    | [] => Ok(Belt.List.reverse(memo))
    | [Error(_) as err, ..._] => err
    | [Ok(x), ...xs] => join(~memo=[x, ...memo], xs);

  let all = (list, f) => Belt.List.map(list, f) |> join;
};

module StringMap = {
  include Map.Make(String);
  exception MissingKey(string);

  let findExn = (key, t) =>
    try (find(key, t)) {
    | Not_found => raise(MissingKey(key))
    };

  let find = (k, t) =>
    try (Some(findExn(k, t))) {
    | MissingKey(_) => None
    };
};

type variableList = list((string, Language.Ast.constValue));
type variableMap = StringMap.t(Language.Ast.constValue);
type fragmentMap = StringMap.t(Language.Ast.fragmentDefinition);

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

module type IO = {
  type t(+'a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
};

let id: 'a. 'a => 'a = x => x;

module Make = (Io: IO) => {
  module Io = {
    include Io;
    let ok = x => return(Belt.Result.Ok(x));
    let error = x => return(Belt.Result.Error(x));
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
          | Belt.Result.Ok(x') => f(x')
          | Error(_) as err => return(err),
        );

      let mapError = (x, f) =>
        map(
          x,
          fun
          | Belt.Result.Ok(_) as ok => ok
          | Error(err) => Error(f(err)),
        );

      let map = (x, f) =>
        map(
          x,
          fun
          | Belt.Result.Ok(x') => Belt.Result.Ok(f(x'))
          | Error(_) as err => err,
        );
    };

    let mapP = (list, f) => List.map(f, list) |> all;

    module Infix = {
      let (>>|) = map;
      let (>>=?) = Result.bind;
    };
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
    lift: 'a => Io.t(Belt.Result.t('out, string)),
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

  // Execution

  type executionContext('ctx) = {
    schema: t('ctx),
    operation: Language.Ast.operationDefinition,
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
    | `Mutations_not_configured
    | `Subscriptions_not_configured
    | `No_operation_found
    | `Operation_name_required
    | `Operation_not_found
  ];

  type executionResult = {data: Language.Ast.constValue};

  module ArgEval = {
    open Arg;

    let rec valueToConstValue: (variableMap, Language.Ast.value) => Language.Ast.constValue =
      variableMap =>
        fun
        | `Null => `Null
        | `Int(_) as i => i
        | `Float(_) as f => f
        | `String(_) as s => s
        | `Boolean(_) as b => b
        | `Enum(_) as e => e
        | `Variable(v) => StringMap.findExn(v, variableMap)
        | `List(xs) => `List(Belt.List.map(xs, valueToConstValue(variableMap)))
        | `Map(props) => {
            let props' =
              Belt.List.map(props, ((name, value)) =>
                (name, valueToConstValue(variableMap, value))
              );
            `Map(props');
          };

    let rec stringOfConstValue: Language.Ast.constValue => string = (
      fun
      | `Null => "null"
      | `Int(i) => string_of_int(i)
      | `Float(f) => string_of_float(f)
      | `String(s) => Printf.sprintf("\"%s\"", s)
      | `Boolean(b) => string_of_bool(b)
      | `Enum(e) => e
      | `List(l) => {
          let values = Belt.List.map(l, i => stringOfConstValue(i));
          Printf.sprintf("[%s]", String.concat(", ", values));
        }
      | `Map(a) => {
          let values =
            Belt.List.map(a, ((k, v)) => Printf.sprintf("%s: %s", k, stringOfConstValue(v)));

          Printf.sprintf("{%s}", String.concat(", ", values));
        }:
        Language.Ast.constValue => string
    );

    let rec stringOfArgType: type a. argType(a) => string =
      fun
      | Scalar(a) => Printf.sprintf("%s!", a.name)
      | InputObject(a) => Printf.sprintf("%s!", a.name)
      | Enum(a) => Printf.sprintf("%s!", a.name)
      | List(a) => Printf.sprintf("[%s]!", stringOfArgType(a))
      | Nullable(Scalar(a)) => Printf.sprintf("%s", a.name)
      | Nullable(InputObject(a)) => Printf.sprintf("%s", a.name)
      | Nullable(Enum(a)) => Printf.sprintf("%s", a.name)
      | Nullable(List(a)) => Printf.sprintf("[%s]", stringOfArgType(a))
      | Nullable(Nullable(_)) => "";

    let evalArgError = (~fieldType="field", ~fieldName, ~argName, argType, value) => {
      let foundStr =
        switch (value) {
        | Some(v) => Printf.sprintf("found %s", stringOfConstValue(v))
        | None => "but not provided"
        };

      Printf.sprintf(
        "Argument `%s` of type `%s` expected on %s `%s`, %s.",
        argName,
        stringOfArgType(argType),
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
          list((string, Language.Ast.value)),
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
              let value = Belt.List.getAssoc(key_values, arg.name, (==));
              let constValue = Belt.Option.map(value, valueToConstValue(variableMap));
              evalArg(
                variableMap,
                ~fieldType?,
                ~fieldName,
                ~argName=arg.name,
                arg.typ,
                constValue,
              )
              ->Belt.Result.flatMap(coerced =>
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
          argType(a),
          option(Language.Ast.constValue)
        ) =>
        Result.t(a, string) =
      (variableMap, ~fieldType=?, ~fieldName, ~argName, typ, value) =>
        switch (typ, value) {
        | (Nullable(_), None) => Ok(None)
        | (Nullable(_), Some(`Null)) => Ok(None)
        | (_, None) => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, value))
        | (_, Some(`Null)) =>
          Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, value))
        | (Nullable(typ), Some(value)) =>
          evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ, Some(value))
          ->Belt.Result.map(value => Some(value))
        | (Scalar(s), Some(value)) =>
          switch (s.parse(value)) {
          | Ok(coerced) => Ok(coerced)
          | Error(_) => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, Some(value)))
          }
        | (InputObject(o), Some(value)) =>
          switch (value) {
          | `Map((props: list((string, Language.Ast.constValue)))) =>
            evalArgList(
              variableMap,
              ~fieldType?,
              ~fieldName,
              o.fields,
              (props :> list((string, Language.Ast.value))),
              o.coerce,
            )
          | _ => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, Some(value)))
          }
        | (List(typ), Some(value)) =>
          switch (value) {
          | `List(values) =>
            let option_values = Belt.List.map(values, x => Some(x));
            Result.all(
              option_values,
              evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ),
            );
          | value =>
            evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ, Some(value))
            ->Belt.Result.map((coerced) => ([coerced]: a))
          }
        | (Enum(enum), Some(value)) =>
          switch (value) {
          | `Enum(v)
          | `String(v) =>
            switch (Belt.List.getBy(enum.values, enum_value => enum_value.name == v)) {
            | Some(enum_value) => Ok(enum_value.value)
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

  open Io.Infix;

  let matchesTypeCondition = (typeCondition: string, obj: obj('ctx, 'src)) =>
    typeCondition == obj.name
    || Belt.List.some(obj.abstracts^, abstract => abstract.name == typeCondition);

  let rec collectFields:
    (fragmentMap, obj('ctx, 'src), list(Language.Ast.selection)) =>
    Belt.Result.t(list(Language.Ast.field), string) =
    (fragmentMap, obj, selectionSet) =>
      selectionSet
      ->Belt.List.map(
          fun
          | Language.Ast.Field(field) => Belt.Result.Ok([field])
          | Language.Ast.FragmentSpread(fragmentSpread) =>
            switch (StringMap.find(fragmentSpread.name, fragmentMap)) {
            | Some({typeCondition, selectionSet}) when matchesTypeCondition(typeCondition, obj) =>
              collectFields(fragmentMap, obj, selectionSet)
            | _ => Ok([])
            }
          | Language.Ast.InlineFragment(inlineFragment) =>
            collectFields(fragmentMap, obj, inlineFragment.selectionSet),
        )
      ->Result.join
      ->Result.map(Belt.List.flatten);

  let fieldName: Language.Ast.field => string =
    fun
    | {alias: Some(alias)} => alias
    | field => field.name;

  let getObjField = (fieldName: string, obj: obj('ctx, 'src)): option(field('ctx, 'src)) =>
    obj.fields |> Lazy.force |> Belt.List.getBy(_, (Field(field)) => field.name == fieldName);


  let rec resolveValue:
    type ctx src.
      (executionContext(ctx), src, Language.Ast.field, typ(ctx, src)) =>
      Io.t(Belt.Result.t(Language.Ast.constValue, [> resolveError])) =
    (executionContext, src, field, typ) =>
      switch (typ) {
      | Nullable(typ') =>
        switch (src) {
        | Some(src') => resolveValue(executionContext, src', field, typ')
        | None => Io.ok(`Null)
        }
      | Scalar(scalar) => Io.ok(scalar.serialize(src))
      | Enum(enum) =>
        switch (Belt.List.getBy(enum.values, enumValue => enumValue.value == src)) {
        | Some(enumValue) => Io.ok(`String(enumValue.name))
        | None => Io.ok(`Null)
        }
      | Object(obj) =>
        switch (collectFields(executionContext.fragmentMap, obj, field.selectionSet)) {
        | Ok(fields) => resolveFields(executionContext, src, obj, fields)
        | Error(e) => Io.error(`ArgumentError(e))
        }
      | List(typ') =>
        Belt.List.map(src, srcItem => resolveValue(executionContext, srcItem, field, typ'))
        ->Io.all
        ->Io.map(Result.join)
        ->Io.Result.map(list => `List(list))
      | Abstract(_) =>
        let AbstractValue((typ', src')) = src;
        resolveValue(executionContext, src', field, typ');
      }

  and resolveField:
    type ctx src.
      (executionContext(ctx), src, Language.Ast.field, field(ctx, src)) =>
      Io.t(Belt.Result.t((string, Language.Ast.constValue), [> resolveError])) =
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
        fieldDef.lift(unlifted)->Io.Result.mapError(err => `ResolveError((err, [])))
        >>=? (resolved => resolveValue(executionContext, resolved, field, fieldDef.typ))
        >>| (
          fun
          | Ok(value) => Result.Ok((name, value))
          | Error(`ArgumentError(_) | `ValidationError(_)) as error => error
          | Error(`ResolveError(_)) as error =>
            switch (fieldDef.typ) {
            | Nullable(_) => Ok((name, `Null))
            | _ => error
            }
        )

      | Error(err) => Io.error(`ArgumentError(err))
      };
    }

  and resolveFields:
    type ctx src.
      (executionContext(ctx), src, obj(ctx, src), list(Language.Ast.field)) =>
      Io.t(Belt.Result.t(Language.Ast.constValue, [> resolveError])) =
    (executionContext, src, obj, fields) => {
      Io.mapP(fields, field =>
        switch (getObjField(field.name, obj)) {
        | Some(objField) => resolveField(executionContext, src, field, objField)
        | None =>
          let err =
            Printf.sprintf("Field '%s' is not defined on type '%s'", field.name, obj.name);
          Io.error(`ValidationError(err));
        }
      )
      ->Io.map(Result.join)
      ->Io.Result.map(assocList => `Map(assocList));
    };

  let executeOperation =
      (executionContext: executionContext('ctx), operation: Language.Ast.operationDefinition)
      : Io.t(Belt.Result.t(Language.Ast.constValue, [> executeError])) =>
    switch (operation.operationType) {
    | Query =>
      /* TODO: Make parallell */

      Io.return(
        collectFields(
          executionContext.fragmentMap,
          executionContext.schema.query,
          operation.selectionSet,
        ),
      )
      ->Io.Result.mapError(e => `ArgumentError(e))
      >>=? (
        fields => (
          resolveFields(executionContext, (), executionContext.schema.query, fields):
            Io.t(Belt.Result.t(Language.Ast.constValue, resolveError)) :>
            Io.t(Belt.Result.t(Language.Ast.constValue, [> executeError]))
        )
      )
    | Mutation =>
      /* TODO: Ensure Sequencial */
      Io.return(
        collectFields(
          executionContext.fragmentMap,
          executionContext.schema.mutation,
          operation.selectionSet,
        ),
      )
      ->Io.Result.mapError(e => `ArgumentError(e))
      >>=? (
        fields => (
          resolveFields(executionContext, (), executionContext.schema.mutation, fields):
            Io.t(Belt.Result.t(Language.Ast.constValue, resolveError)) :>
            Io.t(Belt.Result.t(Language.Ast.constValue, [> executeError]))
        )
      )
    | _ => failwith("Subscription Not implemented")
    };

  let collectOperations = (document: Language.Ast.document) =>
    Belt.List.reduceReverse(document.definitions, [], (list, x) =>
      switch (x) {
      | Language.Ast.OperationDefinition(operation) => [operation, ...list]
      | _ => list
      }
    );

  let collectFragments = (document: Language.Ast.document) =>
    Belt.List.reduceReverse(document.definitions, StringMap.empty, (fragments, x) =>
      switch (x) {
      | Language.Ast.FragmentDefinition(fragment) =>
        fragments |> StringMap.add(fragment.name, fragment)
      | _ => fragments
      }
    );

  let okResponse = data => {
    `Map([("data", data)]);
  };

  let errorResponse = (~path=?, msg): Language.Ast.constValue => {
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
            ("path", `List(Belt.List.map(path', s => `String(s)))),
          ]),
        ]),
      ),
    ]);
  };

  let execute =
      (
        ~variables: variableList=[],
        ~document: Language.Ast.document,
        schema: t('ctx),
        ~ctx: 'ctx,
      ) => {
    let operations = collectOperations(document);
    let fragmentMap = collectFragments(document);

    let variableMap =
      Belt.List.reduce(variables, StringMap.empty, (map, (name, value)) =>
        StringMap.add(name, value, map)
      );

    let result =
      Belt.List.map(
        operations,
        operation => {
          let executionContext = {schema, fragmentMap, operation, variableMap, ctx};
          executeOperation(executionContext, operation);
        },
      )
      |> Belt.List.headExn;

    result
    >>| (
      fun
      | Ok(res) => okResponse(res)
      | Error(`No_operation_found) => errorResponse("No operation found")
      | Error(`Operation_not_found) => errorResponse("Operation not found")
      | Error(`Operation_name_required) => errorResponse("Operation name required")
      | Error(`Subscriptions_not_configured) => errorResponse("Subscriptions not configured")
      | Error(`Mutations_not_configured) => errorResponse("Mutations not configured")
      | Error(`ValidationError(msg)) => errorResponse(msg)
      | Error(`ArgumentError(msg)) => errorResponse(msg)
      | Error(`ResolveError(msg, path)) => errorResponse(msg, ~path)
    );
  };

  let rec constValueToJson: Language.Ast.constValue => Js.Json.t =
    fun
    | `String(string)
    | `Enum(string) => Js.Json.string(string)
    | `Float(float) => Js.Json.number(float)
    | `Int(int) => Js.Json.number(float_of_int(int))
    | `Boolean(bool) => Js.Json.boolean(bool)
    | `List(list) =>
      Belt.List.map(list, item => constValueToJson(item)) |> Array.of_list |> Js.Json.array
    | `Map(assocList) => {
        let dict =
          Belt.List.reduceReverse(
            assocList,
            Js.Dict.empty(),
            (dict, (name, value)) => {
              Js.Dict.set(dict, name, constValueToJson(value));
              dict;
            },
          );
        Js.Json.object_(dict);
      }
    | `Null => Js.Json.null;

  let resultToJson: Io.t(Language.Ast.constValue) => Io.t(Js.Json.t) =
    result => Io.map(result, constValueToJson);
};
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

module Io = Schema.Io;

type variableList = list((string, Language.Ast.constValue));
type variableMap = StringMap.t(Language.Ast.constValue);
type fragmentMap = StringMap.t(Language.Ast.fragmentDefinition);

type executionContext('ctx) = {
  schema: Schema.t('ctx),
  operation: Language.Ast.operationDefinition,
  fragmentMap,
  variableMap,
  ctx: 'ctx,
};

type result('a, 'b) = Schema.result('a, 'b);
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

open Schema.Io.Infix;

module Arg = {
  open Schema.Arg;

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
            evalArg(variableMap, ~fieldType?, ~fieldName, ~argName=arg.name, arg.typ, constValue)
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
      | (_, Some(`Null)) => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, value))
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

let matchesTypeCondition = (typeCondition: string, obj: Schema.obj('ctx, 'src)) =>
  typeCondition == obj.name
  || Belt.List.some(obj.abstracts^, abstract => abstract.name == typeCondition);

let rec collectFields:
  (fragmentMap, Schema.obj('ctx, 'src), list(Language.Ast.selection)) =>
  result(list(Language.Ast.field), string) =
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

let getObjField =
    (fieldName: string, obj: Schema.obj('ctx, 'src)): option(Schema.field('ctx, 'src)) =>
  obj.fields
  |> Lazy.force
  |> Belt.List.getBy(_, (Schema.Field(field)) => field.name == fieldName);


let rec resolveValue:
  type ctx src.
    (executionContext(ctx), src, Language.Ast.field, Schema.typ(ctx, src)) =>
    Io.t(result(Language.Ast.constValue, [> resolveError])) =
  (executionContext, src, field, typ) =>
    switch (typ) {
    | Schema.Nullable(typ') =>
      switch (src) {
      | Some(src') => resolveValue(executionContext, src', field, typ')
      | None => Io.ok(`Null)
      }
    | Schema.Scalar(scalar) => Io.ok(scalar.serialize(src))
    | Schema.Enum(enum) =>
      switch (Belt.List.getBy(enum.values, enumValue => enumValue.value == src)) {
      | Some(enumValue) => Io.ok(`String(enumValue.name))
      | None => Io.ok(`Null)
      }
    | Schema.Object(obj) =>
      switch (collectFields(executionContext.fragmentMap, obj, field.selectionSet)) {
      | Ok(fields) => resolveFields(executionContext, src, obj, fields)
      | Error(e) => Io.error(`ArgumentError(e))
      }
    | Schema.List(typ') =>
      Belt.List.map(src, srcItem => resolveValue(executionContext, srcItem, field, typ'))
      ->Io.all
      ->Io.map(Result.join)
      ->Io.Result.map(list => `List(list))
    | Schema.Abstract(_) =>
      let Schema.AbstractValue((typ', src')) = src;
      resolveValue(executionContext, src', field, typ');
    }

and resolveField:
  type ctx src.
    (executionContext(ctx), src, Language.Ast.field, Schema.field(ctx, src)) =>
    Io.t(result((string, Language.Ast.constValue), [> resolveError])) =
  (executionContext, src, field, Schema.Field(fieldDef)) => {
    let name = fieldName(field);
    let resolver = fieldDef.resolve(executionContext.ctx, src);

    switch (
      Arg.evalArgList(
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
          | Schema.Nullable(_) => Ok((name, `Null))
          | _ => error
          }
      )

    | Error(err) => Io.error(`ArgumentError(err))
    };
  }

and resolveFields:
  type ctx src.
    (executionContext(ctx), src, Schema.obj(ctx, src), list(Language.Ast.field)) =>
    Io.t(result(Language.Ast.constValue, [> resolveError])) =
  (executionContext, src, obj, fields) => {
    Io.mapP(fields, field =>
      switch (getObjField(field.name, obj)) {
      | Some(objField) => resolveField(executionContext, src, field, objField)
      | None =>
        let err = Printf.sprintf("Field '%s' is not defined on type '%s'", field.name, obj.name);
        Io.error(`ValidationError(err));
      }
    )
    ->Io.map(Result.join)
    ->Io.Result.map(assocList => `Map(assocList));
  };

let executeOperation =
    (executionContext: executionContext('ctx), operation: Language.Ast.operationDefinition)
    : Io.t(result(Language.Ast.constValue, [> executeError])) =>
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
          Io.t(result(Language.Ast.constValue, resolveError)) :>
          Io.t(result(Language.Ast.constValue, [> executeError]))
      )
    )
  | Mutation =>
    /* TODO: Ensure Sequencial */
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
        resolveFields(executionContext, (), executionContext.schema.mutation, fields):
          Io.t(result(Language.Ast.constValue, resolveError)) :>
          Io.t(result(Language.Ast.constValue, [> executeError]))
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
  `Map([
    ("data", data)
  ]);
}

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
      schema: Schema.t('ctx),
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
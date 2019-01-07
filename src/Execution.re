module Result = {
  include Belt.Result;

  module Operators = {
    let (>>=) = Belt.Result.flatMap;
    let (>>|) = Belt.Result.map;
  };

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

open Result.Operators;

type variableMap = StringMap.t(Language.Ast.constValue);
type fragmentMap = StringMap.t(Language.Ast.fragmentDefinition);

type executionContext = {
  schema: Schema.t,
  operation: Language.Ast.operationDefinition,
  fragmentMap,
  variableMap,
};

exception ResolveError(string);

type executionErorr =
  | OperationNotFound;

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
      Belt.Result.t(a, string) =
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
            >>= (
              coerced =>
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
      Belt.Result.t(a, string) =
    (variableMap, ~fieldType=?, ~fieldName, ~argName, typ, value) =>
      switch (typ, value) {
      | (Nullable(_), None) => Ok(None)
      | (Nullable(_), Some(`Null)) => Ok(None)
      | (_, None) => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, value))
      | (_, Some(`Null)) => Error(evalArgError(~fieldType?, ~fieldName, ~argName, typ, value))
      | (Nullable(typ), Some(value)) =>
        evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ, Some(value))
        >>| (value => Some(value))
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
          >>| ((coerced) => ([coerced]: a))
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

let matchesTypeCondition = (typeCondition: string, obj: Schema.obj('src)) =>
  typeCondition == obj.name;

let rec collectFields:
  (fragmentMap, Schema.obj('src), list(Language.Ast.selection)) => list(Language.Ast.field) =
  (fragmentMap, obj, selectionSet) =>
    selectionSet
    |> Belt.List.map(
         _,
         fun
         | Language.Ast.Field(field) => [field]
         | Language.Ast.FragmentSpread(fragmentSpread) =>
           switch (StringMap.find(fragmentSpread.name, fragmentMap)) {
           | Some({typeCondition, selectionSet}) when matchesTypeCondition(typeCondition, obj) =>
             collectFields(fragmentMap, obj, selectionSet)
           | _ => []
           }
         | Language.Ast.InlineFragment(inlineFragment) =>
           collectFields(fragmentMap, obj, inlineFragment.selectionSet),
       )
    |> Belt.List.flatten;

let fieldName: Language.Ast.field => string =
  fun
  | {alias: Some(alias)} => alias
  | field => field.name;

let getObjField = (fieldName: string, obj: Schema.obj('src)): option(Schema.field('src)) =>
  obj.fields
  |> Lazy.force
  |> Belt.List.getBy(_, (Schema.Field(field)) => field.name == fieldName);


let rec resolveValue:
  type src.
    (executionContext, src, Language.Ast.field, Schema.typ(src)) => Language.Ast.constValue =
  (executionContext, src, field, typ) =>
    switch (typ) {
    | Schema.Scalar(scalar) => scalar.serialize(src)
    | Schema.Enum(enum) =>
      switch (Belt.List.getBy(enum.values, enumValue => enumValue.value == src)) {
      | Some(enumValue) => `String(enumValue.name)
      | None => `Null
      }
    | Schema.Object(obj) =>
      let fields = collectFields(executionContext.fragmentMap, obj, field.selectionSet);
      `Map(resolveFields(executionContext, src, obj, fields));
    | Schema.List(typ') =>
      `List(Belt.List.map(src, srcItem => resolveValue(executionContext, srcItem, field, typ')))
    | _ => failwith("resolve type Not implemented")
    }

and resolveField:
  type src.
    (executionContext, src, Language.Ast.field, Schema.field(src)) =>
    (string, Language.Ast.constValue) =
  (executionContext, src, field, Schema.Field(fieldDef)) => {
    let name = fieldName(field);
    let resolver = fieldDef.resolve(src);

    switch (
      Arg.evalArgList(
        StringMap.empty,
        ~fieldName=fieldDef.name,
        fieldDef.arguments,
        field.arguments,
        resolver,
      )
    ) {
    | Ok(unlifted) =>
      let lifted = fieldDef.lift(unlifted);
      (name, resolveValue(executionContext, lifted, field, fieldDef.typ));
    | Error(e) => raise(ResolveError(e))
    };
  }

and resolveFields:
  type src.
    (executionContext, src, Schema.obj(src), list(Language.Ast.field)) =>
    list((string, Language.Ast.constValue)) =
  (executionContext, src, obj, fields) =>
    Belt.List.map(fields, field =>
      switch (getObjField(field.name, obj)) {
      | Some(objField) => resolveField(executionContext, src, field, objField)
      | None => failwith("Field " ++ field.name ++ "is not defined on type " ++ obj.name)
      }
    );

let executeOperation =
    (schema: Schema.t, fragmentMap: fragmentMap, operation: Language.Ast.operationDefinition)
    : Language.Ast.constValue =>
  switch (operation.operationType) {
  | Query =>
    let fields = collectFields(fragmentMap, schema.query, operation.selectionSet);
    `Map(
      resolveFields(
        {schema, fragmentMap, operation, variableMap: StringMap.empty},
        (),
        schema.query,
        fields,
      ),
    );

  | _ => failwith("Mutation/Subscription Not implemented")
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

let execute =
    (~_variables=StringMap.empty, schema: Schema.t, ~document: Language.Ast.document)
    : Language.Ast.constValue => {
  let operations = collectOperations(document);
  let fragments = collectFragments(document);
  let data =
    operations |> Belt.List.map(_, executeOperation(schema, fragments)) |> Belt.List.headExn;
  `Map([("data", data)]);
};

let rec resultToJson: Language.Ast.constValue => Js.Json.t =
  fun
  | `String(string)
  | `Enum(string) => Js.Json.string(string)
  | `Float(float) => Js.Json.number(float)
  | `Int(int) => Js.Json.number(float_of_int(int))
  | `Boolean(bool) => Js.Json.boolean(bool)
  | `List(list) =>
    list |> Belt.List.map(_, item => resultToJson(item)) |> Array.of_list |> Js.Json.array
  | `Map(assocList) => {
      let dict = Js.Dict.empty();
      Belt.List.forEach(assocList, ((name, value)) =>
        Js.Dict.set(dict, name, resultToJson(value))
      );
      Js.Json.object_(dict);
    }
  | `Null => Js.Json.null;
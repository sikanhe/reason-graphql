module StringMap = Map.Make(String);
open Belt;
type variables = StringMap.t(Ast.value);
type fragments = StringMap.t(Ast.fragmentDefinition);

type executionContext = {
  schema: Schema.t,
  operation: Ast.operationDefinition,
  fragments,
  variables,
};

type result('a, 'b) =
  | Ok('a)
  | Error('b);

exception ResolveError(string);

type executionErorr =
  | OperationNotFound;

let matchesTypeCondition = (typeCondition: string, obj: Schema.obj('src)) =>
  typeCondition == obj.name;

let rec collectFields: (fragments, Schema.obj('src), list(Ast.selection)) => list(Ast.field) =
  (fragments, obj, selectionSet) =>
    selectionSet
    ->List.map(
        fun
        | Ast.Field(field) => [field]
        | Ast.FragmentSpread(fragmentSpread) =>
          switch (StringMap.find(fragmentSpread.name, fragments)) {
          | {typeCondition, selectionSet} when matchesTypeCondition(typeCondition, obj) =>
            collectFields(fragments, obj, selectionSet)
          | exception Not_found => []
          | _ => []
          }
        | Ast.InlineFragment(inlineFragment) =>
          collectFields(fragments, obj, inlineFragment.selectionSet),
      )
    ->List.flatten;

let fieldName: Ast.field => string =
  fun
  | {alias: Some(alias)} => alias
  | field => field.name;

let getObjField = (fieldName: string, obj: Schema.obj('src)): option(Schema.field('src)) =>
  obj.fields->Lazy.force->List.getBy((Schema.Field(field)) => field.name == fieldName);


let rec resolveValue:
  type src. (executionContext, src, Ast.field, Schema.typ(src)) => Ast.constValue =
  (executionContext, src, field, typ) =>
    switch (typ) {
    | Schema.Scalar(scalar) => scalar.serialize(src)
    | Schema.Enum(enum) =>
      switch (List.getBy(enum.values, enumValue => enumValue.value == src)) {
      | Some(enumValue) => `String(enumValue.name)
      | None => `Null
      }
    | Schema.Object(obj) =>
      let fields = collectFields(executionContext.fragments, obj, field.selectionSet);
      `Map(resolveFields(executionContext, src, obj, fields));
    | Schema.List(typ') =>
      `List(List.map(src, srcItem => resolveValue(executionContext, srcItem, field, typ')))
    | _ => failwith("resolve type Not implemented")
    }

and resolveField:
  type src. (executionContext, src, Ast.field, Schema.field(src)) => (string, Ast.constValue) =
  (executionContext, src, field, Schema.Field(fieldDef)) => {
    let name = fieldName(field);
    let resolver = fieldDef.resolve(src);

    switch (
      Schema.Arg.eval_arglist(
        StringMap.empty,
        ~field_name=fieldDef.name,
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
    (executionContext, src, Schema.obj(src), list(Ast.field)) => list((string, Ast.constValue)) =
  (executionContext, src, obj, fields) =>
    List.map(fields, (field: Ast.field) =>
      switch (getObjField(field.name, obj)) {
      | Some(objField) => resolveField(executionContext, src, field, objField)
      | None => failwith("Field " ++ field.name ++ "is not defined on type " ++ obj.name)
      }
    );

let executeOperation =
    (schema: Schema.t, fragments: fragments, operation: Ast.operationDefinition): Ast.constValue =>
  switch (operation.operationType) {
  | Query =>
    let fields = collectFields(fragments, schema.query, operation.selectionSet);
    `Map(
      resolveFields(
        {schema, fragments, operation, variables: StringMap.empty},
        (),
        schema.query,
        fields,
      ),
    );

  | _ => failwith("Mutation/Subscription Not implemented")
  };

let collectOperations = (document: Ast.document) =>
  List.reduceReverse(document.definitions, [], (list, x) =>
    switch (x) {
    | Ast.OperationDefinition(operation) => [operation, ...list]
    | _ => list
    }
  );

let collectFragments = (document: Ast.document) =>
  List.reduceReverse(document.definitions, StringMap.empty, (fragments, x) =>
    switch (x) {
    | Ast.FragmentDefinition(fragment) => fragments |> StringMap.add(fragment.name, fragment)
    | _ => fragments
    }
  );

let execute =
    (~variables=StringMap.empty, schema: Schema.t, ~document: Ast.document): Ast.constValue => {
  let operations = collectOperations(document);
  let fragments = collectFragments(document);
  let data = operations->List.map(executeOperation(schema, fragments))->List.headExn;
  `Map([("data", data)]);
};
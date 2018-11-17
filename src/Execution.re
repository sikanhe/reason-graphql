open Schema;
module StringMap = Map.Make(String);

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

type executionErorr =
  | OperationNotFound;

let matchesTypeCondition = (typeCondition: string, obj: obj('src)) => typeCondition == obj.name;

let rec collectFields: (fragments, obj('src), list(Ast.selection)) => list(Ast.field) =
  (fragments, obj, selectionSet) =>
    selectionSet
    |> List.map(
         fun
         | Ast.Field(field) => [field]
         | Ast.FragmentSpread(fragmentSpread) =>
           switch (StringMap.find(fragmentSpread.name, fragments)) {
           | {typeCondition, selectionSet} when typeCondition == obj.name =>
             collectFields(fragments, obj, selectionSet)
           | exception Not_found => []
           | _ => []
           }
         | Ast.InlineFragment(inlineFragment) =>
           collectFields(fragments, obj, inlineFragment.selectionSet),
       )
    |> List.concat;

let fieldName: Ast.field => string =
  fun
  | {alias: Some(alias)} => alias
  | {name} => name;

let getObjField = (fieldName: string, obj: obj('src)): field('src) =>
  obj.fields |> Lazy.force |> List.find((Field(field)) => field.name == fieldName);

let rec resolveType: type src. (executionContext, src, Ast.field, typ(src)) => value =
  (executionContext, src, field, typ) =>
    switch (typ) {
    | Scalar(scalar) => scalar.serialize(src)
    | Object(obj) =>
      let fields = collectFields(executionContext.fragments, obj, field.selectionSet);
      `Object(resolveFields(executionContext, src, obj, fields));
    | List(typ') =>
      `List(src |> List.map(srcItem => resolveType(executionContext, srcItem, field, typ')))
    | _ => failwith("resolve type Not implemented")
    }
and resolveField:
  type src. (executionContext, src, Ast.field, Schema.field(src)) => (string, value) =
  (executionContext, src, field, Field(fieldDef)) => {
    let name = fieldName(field);
    let out = fieldDef.resolve(src);
    (name, resolveType(executionContext, out, field, fieldDef.typ));
  }
and resolveFields:
  type src. (executionContext, src, obj(src), list(Ast.field)) => list((string, value)) =
  (executionContext, src, obj, fields) =>
    List.map(
      (field: Ast.field) => {
        let objField: field(src) = getObjField(field.name, obj);
        resolveField(executionContext, src, field, objField);
      },
      fields,
    );

let executeOperation =
    (schema: t, fragments: fragments, operation: Ast.operationDefinition): value =>
  switch (operation.operationType) {
  | Query =>
    let fields = collectFields(fragments, schema.query, operation.selectionSet);
    `Object(
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
  List.fold_left(
    (list, x) =>
      switch (x) {
      | Ast.OperationDefinition(operation) => [operation, ...list]
      | _ => list
      },
    [],
    document.definitions,
  );

let execute = (~variables=StringMap.empty, schema: t, ~document: Ast.document) => {
  let operations = collectOperations(document);
  let fragments = StringMap.empty;
  let [data] = operations |> List.map(executeOperation(schema, fragments));
  `Object([("data", data)]);
};
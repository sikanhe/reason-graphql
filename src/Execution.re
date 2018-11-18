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

let matchesTypeCondition = (typeCondition: string, obj: Schema.obj('src)) =>
  typeCondition == obj.name;

let rec collectFields: (fragments, Schema.obj('src), list(Ast.selection)) => list(Ast.field) =
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

let getObjField = (fieldName: string, obj: Schema.obj('src)): Schema.field('src) =>
  obj.fields |> Lazy.force |> List.find((Schema.Field(field)) => field.name == fieldName);

let rec resolveValue:
  type src. (executionContext, src, Ast.field, Schema.typ(src)) => Schema.value =
  (executionContext, src, field, typ) =>
    switch (typ) {
    | Schema.Scalar(scalar) => scalar.serialize(src)
    | Schema.Enum({values}) =>
      `String(List.find(({Schema.value}) => value == src, values).name)
    | Schema.Object(obj) =>
      let fields = collectFields(executionContext.fragments, obj, field.selectionSet);
      `Map(resolveFields(executionContext, src, obj, fields));
    | Schema.List(typ') =>
      `List(src |> List.map(srcItem => resolveValue(executionContext, srcItem, field, typ')))
    | _ => failwith("resolve type Not implemented")
    }
and resolveField:
  type src. (executionContext, src, Ast.field, Schema.field(src)) => (string, Schema.value) =
  (executionContext, src, field, Schema.Field(fieldDef)) => {
    let name = fieldName(field);
    let out = fieldDef.resolve(src);
    (name, resolveValue(executionContext, out, field, fieldDef.typ));
  }
and resolveFields:
  type src.
    (executionContext, src, Schema.obj(src), list(Ast.field)) => list((string, Schema.value)) =
  (executionContext, src, obj, fields) =>
    List.map(
      (field: Ast.field) => {
        let objField = getObjField(field.name, obj);
        resolveField(executionContext, src, field, objField);
      },
      fields,
    );

let executeOperation =
    (schema: Schema.t, fragments: fragments, operation: Ast.operationDefinition): Schema.value =>
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
  List.fold_left(
    (list, x) =>
      switch (x) {
      | Ast.OperationDefinition(operation) => [operation, ...list]
      | _ => list
      },
    [],
    document.definitions,
  );

let collectFragments = (document: Ast.document) =>
  List.fold_left(
    (fragments, x) =>
      switch (x) {
      | Ast.FragmentDefinition(fragment) => fragments |> StringMap.add(fragment.name, fragment)
      | _ => fragments
      },
    StringMap.empty,
    document.definitions,
  );

let execute =
    (~variables=StringMap.empty, schema: Schema.t, ~document: Ast.document): Schema.value => {
  let operations = collectOperations(document);
  let fragments = collectFragments(document);
  let data = operations |> List.map(executeOperation(schema, fragments)) |> List.hd;
  `Map([("data", data)]);
};
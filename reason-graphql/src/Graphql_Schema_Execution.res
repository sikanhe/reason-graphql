open Graphql_Schema
open Graphql_Language

module StringSet = Set.Make(String)
module StringMap = Belt.Map.String

open Belt

type variableMap = StringMap.t<Ast.constValue>
type fragmentMap = StringMap.t<Ast.fragmentDefinition>
type variableList = list<(string, Ast.constValue)>

module Promise = {
  include Js.Promise

  let map = (f, p) => then_(x => resolve(f(x)), p)

  module Array = {
    let mapParallel = (list: array<'a>, f) => {
      list->Array.map(f)->all
    }

    let mapSerial = (list: array<'a>, f) => {
      let result: array<'b> = []

      let rec loop = list => {
        switch list {
        | [] => resolve(result)
        | [x] =>
          f(x) |> then_(x' => {
            result->Js.Array2.push(x')->ignore
            loop([])
          })
        | array =>
          f(Array.getExn(array, 0)) |> then_(x' => {
            result->Js.Array2.push(x')->ignore
            loop(Js.Array2.sliceFrom(array, 1))
          })
        }
      }

      loop(list)
    }
  }
}

type path = list<string>
type resolveError
external promiseErrorAsResolveError: Promise.error => resolveError = "%identity"
external exnAsResolveError: exn => resolveError = "%identity"

type executionContext<'ctx> = {
  schema: schema<'ctx>,
  operation: Ast.operationDefinition,
  fragmentMap: fragmentMap,
  variableMap: variableMap,
  ctx: 'ctx,
  errors: array<(resolveError, path)>,
}

exception ArgumentError(string)
exception ValidationError(string)
exception ResolveError(resolveError, path)
exception MutationsNotConfigured
exception SubscriptionsNotConfigured
exception NoOperationFound
exception OperationNameRequired
exception UnknownOperationName(string)

module ArgEval = {
  open Arg

  let rec valueToConstValue: (variableMap, Ast.value) => Ast.constValue = (variableMap, x) =>
    switch x {
    | #Null => #Null
    | #Int(_) as i => i
    | #Float(_) as f => f
    | #String(_) as s => s
    | #Boolean(_) as b => b
    | #Enum(_) as e => e
    | #Variable(v) =>
      switch StringMap.get(variableMap, v) {
      | Some(value) => value
      | None => #Null
      }
    | #List(xs) => #List(List.map(xs, valueToConstValue(variableMap)))
    | #Object(props) =>
      let props' = List.map(props, ((name, value)) => (name, valueToConstValue(variableMap, value)))
      #Object(props')
    }

  let rec stringOfConstValue: Ast.constValue => string = (
    x =>
      switch x {
      | #Null => "null"
      | #Int(i) => string_of_int(i)
      | #Float(f) => Js.Float.toString(f)
      | #String(s) => "\"" ++ s ++ "\""
      | #Boolean(b) => string_of_bool(b)
      | #Enum(e) => e
      | #List(l) =>
        let values = List.map(l, i => stringOfConstValue(i))
        let stringList = String.concat(", ", values)
        "[" ++ stringList ++ "]"
      | #Object(a) =>
        let values = List.map(a, ((k, v)) => {
          let v = stringOfConstValue(v)
          k ++ ": " ++ v
        })
        "{" ++ String.concat(", ", values) ++ "}"
      }: Ast.constValue => string
  )

  let rec stringOfArgType:
    type a. argTyp<a> => string =
    x =>
      switch x {
      | Scalar(a) => a.name
      | InputObject(a) => a.name
      | Enum(a) => a.name
      | List(a) => "[" ++ stringOfArgType(a) ++ "]"
      | NonNull(a) => stringOfArgType(a) ++ "!"
      }

  let evalArgError = (~fieldType="field", ~fieldName, ~argName, argTyp, value) => {
    let foundStr = switch value {
    | Some(v) => "found " ++ stringOfConstValue(v)
    | None => "but not provided"
    }

    let argTypeString = stringOfArgType(argTyp)

    raise(
      ArgumentError(j`Argument $argName of type $argTypeString expected on $fieldType "$fieldName", $foundStr.`),
    )
  }

  let rec evalArgList:
    type a. (
      variableMap,
      ~fieldType: string=?,
      ~fieldName: string,
      argList<a>,
      list<(string, Ast.value)>,
    ) => a =
    (variableMap, ~fieldType=?, ~fieldName, argList, key_values) => {
      let rec evalArgListItem:
        type a. arg<a> => a =
        arg => {
          switch arg {
          | DefaultArg(arg) =>
            switch evalArgListItem(
              Arg({name: arg.name, description: arg.description, typ: arg.typ}),
            ) {
            | None => arg.default
            | Some(result) => result
            }
          | Arg({name, typ}) =>
            let value = List.getAssoc(key_values, name, \"=")
            let constValue = Option.map(value, valueToConstValue(variableMap))
            evalArg(variableMap, ~fieldType?, ~fieldName, ~argName=name, typ, constValue)
          }
        }

      switch argList {
      | Empty => ()
      | Arg1(arg) => evalArgListItem(arg)
      | Arg2(a1, a2) => (evalArgListItem(a1), evalArgListItem(a2))
      | Arg3(a1, a2, a3) => (evalArgListItem(a1), evalArgListItem(a2), evalArgListItem(a3))
      }
    }

  and evalArg:
    type a. (
      variableMap,
      ~fieldType: string=?,
      ~fieldName: string,
      ~argName: string,
      argTyp<a>,
      option<Ast.constValue>,
    ) => a =
    (variableMap, ~fieldType=?, ~fieldName, ~argName, typ, value) =>
      switch (typ, value) {
      | (NonNull(_), None) => evalArgError(~fieldType?, ~fieldName, ~argName, typ, value)
      | (NonNull(_), Some(#Null)) => evalArgError(~fieldType?, ~fieldName, ~argName, typ, value)
      | (Scalar(_), None) => None
      | (Scalar(_), Some(#Null)) => None
      | (InputObject(_), None) => None
      | (InputObject(_), Some(#Null)) => None
      | (List(_), None) => None
      | (List(_), Some(#Null)) => None
      | (Enum(_), None) => None
      | (Enum(_), Some(#Null)) => None
      | (NonNull(typ), Some(value)) =>
        switch evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ, Some(value)) {
        | Some(value) => value
        | None => evalArgError(~fieldType?, ~fieldName, ~argName, typ, None)
        }
      | (Scalar(s), Some(value)) =>
        switch s.parse(value) {
        | Ok(coerced) => Some(coerced)
        | Error(_) => evalArgError(~fieldType?, ~fieldName, ~argName, typ, Some(value))
        }
      | (InputObject(o), Some(value)) =>
        switch value {
        | #Object(props: list<(string, Ast.constValue)>) =>
          let value = evalArgList(
            variableMap,
            ~fieldType?,
            ~fieldName,
            o.fields,
            (props :> list<(string, Ast.value)>),
          )
          Some(o.coerce(. value))
        | _ => evalArgError(~fieldType?, ~fieldName, ~argName, typ, Some(value))
        }
      | (List(typ), Some(value)) =>
        switch value {
        | #List(values) =>
          let optionValues = List.map(values, x => Some(x))
          let evaledList = List.map(
            optionValues,
            evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ),
          )
          Some(List.toArray(evaledList))
        | value =>
          let evaled = evalArg(variableMap, ~fieldType?, ~fieldName, ~argName, typ, Some(value))
          Some([evaled])
        }
      | (Enum(enum), Some(value)) =>
        switch value {
        | #Enum(v)
        | #String(v) =>
          switch Belt.List.getBy(enum.values, enumValue => enumValue.name == v) {
          | Some(enumValue) => Some(enumValue.value)
          | None =>
            raise(
              ArgumentError(j`Invalid enum value for argument "$argName" on field "$fieldName"`),
            )
          }
        | _ => raise(ArgumentError(j`Expected enum for argument "$argName" on field "$fieldName"`))
        }
      }
}

let matchesTypeCondition = (typeCondition: string, obj: obj<'ctx, 'src>) =>
  typeCondition == obj.name ||
    Belt.List.some(obj.abstracts.contents, abstract => abstract.name == typeCondition)

let rec shouldIncludeField = (ctx, directives: list<Ast.directive>) =>
  switch directives {
  | list{} => true
  | list{{name: "skip", arguments}, ...rest} => eval_directive(ctx, skipDirective, arguments, rest)
  | list{{name: "include", arguments}, ...rest} =>
    eval_directive(ctx, includeDirective, arguments, rest)
  | list{{name, _}, ..._} => raise(ArgumentError(j`Unknown directive: $name`))
  }
and eval_directive = (ctx, Directive({name, args, resolve, _}), arguments, rest) => {
  switch ArgEval.evalArgList(
    ctx.variableMap,
    ~fieldType="directive",
    ~fieldName=name,
    args,
    arguments,
  )->resolve {
  | #Skip => false
  | #Include => shouldIncludeField(ctx, rest)
  }
}

let rec collectFields: (
  executionContext<'ctx>,
  obj<'ctx, 'src>,
  list<Ast.selection>,
) => array<Ast.field> = (ctx, obj, selectionSet) => {
  let fields = []

  selectionSet
  ->List.toArray
  ->Array.forEach(x =>
    switch x {
    | Ast.Field(field) =>
      if shouldIncludeField(ctx, field.directives) {
        Js.Array2.push(fields, field)->ignore
      }
    | Ast.FragmentSpread(fragmentSpread) =>
      switch StringMap.get(ctx.fragmentMap, fragmentSpread.name) {
      | Some({typeCondition, selectionSet, directives})
        if matchesTypeCondition(typeCondition, obj) =>
        if shouldIncludeField(ctx, directives) {
          Js.Array2.pushMany(fields, collectFields(ctx, obj, selectionSet))->ignore
        }
      | _ => ()
      }
    | Ast.InlineFragment({typeCondition: Some(condition), directives} as inlineFragment)
      if matchesTypeCondition(condition, obj) =>
      if shouldIncludeField(ctx, directives) {
        Js.Array2.pushMany(fields, collectFields(ctx, obj, inlineFragment.selectionSet))->ignore
      }
    | Ast.InlineFragment({typeCondition: _}) => ()
    }
  )

  fields
}

let fieldName: Ast.field => string = x =>
  switch x {
  | {alias: Some(alias)} => alias
  | field => field.name
  }

let getObjField = (fieldName: string, obj: obj<'ctx, 'src>): option<field<'ctx, 'src>> =>
  obj.fields->Lazy.force->Belt.List.getBy((Field(field)) => field.name == fieldName)

let coerceOrNull = (src, f) =>
  switch src {
  | Some(src') => f(src')
  | None => Promise.resolve(#Null)
  }

let rec resolveValue:
  type ctx src. (
    executionContext<ctx>,
    src,
    Ast.field,
    typ<ctx, src>,
  ) => Promise.t<Ast.constValue> =
  (executionContext, src, field, typ) =>
    switch typ {
    | NonNull(typ') => resolveValue(executionContext, Some(src), field, typ')
    | Scalar(scalar) =>
      switch src {
      | Some(src') => Promise.resolve(scalar.serialize(src'))
      | None => Promise.resolve(#Null)
      }
    | Enum(enum) =>
      coerceOrNull(src, src' =>
        switch List.getBy(enum.values, enumValue => enumValue.value == src') {
        | Some(enumValue) => Promise.resolve(#String(enumValue.name))
        | None => Promise.resolve(#Null)
        }
      )
    | Object(obj) =>
      coerceOrNull(src, src' => {
        let fields = collectFields(executionContext, obj, field.selectionSet)
        resolveFields(executionContext, src', obj, fields)
      })
    | List(typ') =>
      coerceOrNull(src, src' => {
        let list = Belt.Array.map(src', srcItem =>
          resolveValue(executionContext, srcItem, field, typ')
        )
        list |> Promise.all |> Promise.then_(list => Promise.resolve(#List(list->List.fromArray)))
      })
    | Abstract(_) =>
      coerceOrNull(src, src' => {
        let AbstractValue((typ', src')) = src'
        resolveValue(executionContext, Some(src'), field, typ')
      })
    }

and resolveField:
  type ctx src. (
    executionContext<ctx>,
    src,
    Ast.field,
    field<ctx, src>,
  ) => Promise.t<(string, Ast.constValue)> =
  (executionContext, src, field, Field(fieldDef)) => {
    let name = fieldName(field)

    try {
      switch ArgEval.evalArgList(
        executionContext.variableMap,
        ~fieldName=fieldDef.name,
        fieldDef.args,
        field.arguments,
      ) {
      | args =>
        try {
          fieldDef.resolve(executionContext.ctx, src, args)
          |> Promise.then_(resolved =>
            resolveValue(executionContext, resolved, field, fieldDef.typ)
          )
          |> Promise.map(resolvedValue => {
            (name, resolvedValue)
          })
          |> Promise.catch(e => {
            switch fieldDef.typ {
            | NonNull(_) => Promise.reject(ResolveError((promiseErrorAsResolveError(e), list{})))
            | _ =>
              executionContext.errors
              ->Js.Array2.push((promiseErrorAsResolveError(e), list{}))
              ->ignore
              Promise.resolve((name, #Null))
            }
          })
        } catch {
        | exn =>
          switch fieldDef.typ {
          | NonNull(_) => Promise.reject(ResolveError((exnAsResolveError(exn), list{})))
          | _ =>
            executionContext.errors->Js.Array2.push((exnAsResolveError(exn), list{}))->ignore
            Promise.resolve((name, #Null))
          }
        }
      }
    } catch {
    | exn => Promise.reject(exn)
    }
  }

and resolveFields:
  type ctx src. (
    executionContext<ctx>,
    src,
    obj<ctx, src>,
    array<Ast.field>,
  ) => Promise.t<Ast.constValue> =
  (executionContext, src, obj, fields) => {
    let mapFields = switch executionContext.operation.operationType {
    | Query
    | Subscription => Promise.Array.mapParallel
    | Mutation => Promise.Array.mapSerial
    }

    mapFields(fields, field =>
      if field.name == "__typename" {
        Promise.resolve((fieldName(field), #String(obj.name)))
      } else {
        switch getObjField(field.name, obj) {
        | Some(objField) => resolveField(executionContext, src, field, objField)
        | None =>
          let fieldName = field.name
          let objName = obj.name
          Promise.reject(ValidationError(j`Field "$fieldName" is not defined on type "$objName"`))
        }
      }
    ) |> Promise.then_(assocList => Promise.resolve(#Object(assocList->List.fromArray)))
  }

let executeOperation = (
  executionContext: executionContext<'ctx>,
  operation: Ast.operationDefinition,
): Promise.t<Ast.constValue> =>
  switch operation.operationType {
  | Query =>
    let fields = collectFields(
      executionContext,
      executionContext.schema.query,
      operation.selectionSet,
    )
    resolveFields(executionContext, (), executionContext.schema.query, fields)
  | Mutation =>
    switch executionContext.schema.mutation {
    | Some(mutation) =>
      let fields = collectFields(executionContext, mutation, operation.selectionSet)
      resolveFields(executionContext, (), mutation, fields)
    | None => Promise.reject(MutationsNotConfigured)
    }
  | _ => raise(SubscriptionsNotConfigured)
  }

let collectOperations = (document: Ast.document) =>
  Belt.List.reduceReverse(document.definitions, list{}, (list, x) =>
    switch x {
    | Ast.Operation(operation) => list{operation, ...list}
    | _ => list
    }
  )

let collectFragments = (document: Ast.document) =>
  Belt.List.reduceReverse(document.definitions, StringMap.empty, (fragmentMap, x) =>
    switch x {
    | Ast.Fragment(fragment) => StringMap.set(fragmentMap, fragment.name, fragment)
    | _ => fragmentMap
    }
  )

exception FragmentCycle(list<string>)

let rec validateFragments = fragmentMap => {
  StringMap.forEach(fragmentMap, (name, _) => validateFragment(fragmentMap, StringSet.empty, name))
  fragmentMap
}

and validateFragment = (fragmentMap: fragmentMap, visited, name) =>
  switch StringMap.get(fragmentMap, name) {
  | None => ()
  | Some(fragment) if StringSet.mem(fragment.name, visited) =>
    raise(FragmentCycle(StringSet.elements(visited)))
  | Some(fragment) =>
    let visited' = StringSet.add(fragment.name, visited)
    Belt.List.forEach(fragment.selectionSet, validateFragmentSelection(fragmentMap, visited'))
  }

and validateFragmentSelection = (fragmentMap, visited, selection) =>
  switch selection {
  | Field(field) =>
    Belt.List.forEach(field.selectionSet, validateFragmentSelection(fragmentMap, visited))
  | InlineFragment(inlineFragment) =>
    Belt.List.forEach(inlineFragment.selectionSet, validateFragmentSelection(fragmentMap, visited))
  | FragmentSpread(fragmentSpread) => validateFragment(fragmentMap, visited, fragmentSpread.name)
  }

let collectAndValidateFragments = doc => {
  let fragments = collectFragments(doc)
  validateFragments(fragments)
}

let buildResponse = (data, errors: array<(resolveError, path)>): Ast.constValue => {
  switch errors {
  | [] => #Object(list{("data", data)})
  | errors =>
    let errorsList =
      errors
      ->List.fromArray
      ->List.map(((resolveError, path)) => {
        let path = #List(List.map(path, s => #String(s)))
        switch Js.Exn.anyToExnInternal(resolveError) {
        | ValidationError(msg) => #Object(list{("message", #String(msg)), ("path", path)})
        | Js.Exn.Error(jsError) =>
          let stack = Js.Exn.stack(jsError)
          let msg = Option.getWithDefault(stack, "")
          #Object(list{("message", #String(msg)), ("path", path)})
        | exn => {
            let errorName = Obj.magic(exn)["RE_EXN_ID"]
            let jsError = Obj.magic(exn)["Error"]
            let stack = Js.Exn.stack(jsError)
            let msg = "RE_EX_ID: " ++ errorName ++ ", \n" ++ Option.getWithDefault(stack, "")
            #Object(list{("message", #String(msg)), ("path", path)})
          }
        }
      })
    #Object(list{("data", data), ("errors", #List(errorsList))})
  }
}

let errorResponse = (msg): Ast.constValue => {
  #Object(list{
    ("data", #Null),
    ("errors", #List(list{#Object(list{("message", #String(msg)), ("path", #List(list{}))})})),
  })
}

let selectOperation = (
  ~operationName: option<string>=?,
  operations: list<Ast.operationDefinition>,
) => {
  switch (operations, operationName) {
  | (list{}, _) => raise(NoOperationFound)
  | (list{_, _, ..._}, None) => raise(OperationNameRequired)
  | (list{op}, _) => op
  | (operations, Some(operationName)) =>
    switch List.getBy(operations, op =>
      switch op.name {
      | Some(name) if name == operationName => true
      | _ => false
      }
    ) {
    | Some(op) => op
    | None => raise(UnknownOperationName(operationName))
    }
  }
}

let execute = (
  ~variables: variableList=list{},
  ~document: Ast.document,
  ~operationName: option<string>=?,
  schema: schema<'ctx>,
  ~ctx: 'ctx,
) => {
  let execute' = (schema, ctx, document) => {
    Promise.make((~resolve, ~reject) => {
      let operations = collectOperations(document)
      let fragmentMap = collectAndValidateFragments(document)
      let variableMap = Belt.List.reduce(variables, StringMap.empty, (map, (name, value)) =>
        StringMap.set(map, name, value)
      )

      let schemaWithIntrospection = Graphql_Schema_Introspection.addSchemaField(schema)

      let operation = selectOperation(~operationName?, operations)

      let executionContext = {
        schema: schemaWithIntrospection,
        fragmentMap: fragmentMap,
        operation: operation,
        variableMap: variableMap,
        ctx: ctx,
        errors: [],
      }

      resolve(. (executeOperation(executionContext, operation), executionContext.errors))
    })
  }

  execute'(schema, ctx, document)
  |> Promise.then_(((responsePromise, errors)) => {
    responsePromise |> Promise.map(response => buildResponse(response, errors))
  })
  |> Promise.catch(e => {
    switch (Obj.magic(e): exn) {
    | NoOperationFound => errorResponse("Must provide an operation.")
    | OperationNameRequired =>
      errorResponse("Must provide operation name if query contains multiple operations.")
    | UnknownOperationName(name) => errorResponse("Unknown operation named \"" ++ name ++ "\".")
    | SubscriptionsNotConfigured => errorResponse("Subscriptions not configured")
    | MutationsNotConfigured => errorResponse("Mutations not configured")
    | FragmentCycle(fragmentNames) =>
      let msg = "Fragment cycle detected: " ++ String.concat(" -> ", fragmentNames)
      errorResponse(msg)
    | ArgumentError(msg) => errorResponse(msg)
    | ResolveError(error, path) => buildResponse(#Null, [(error, path)])
    | error => raise(error)
    } |> Promise.resolve
  })
}

let resultToJson: Promise.t<Ast.constValue> => Promise.t<Js.Json.t> = result =>
  Promise.map(Graphql_Json.fromConstValue, result)

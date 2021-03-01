open Graphql_Language_Ast
open Belt

let join = (list, seperator) => List.keep(list, x => x !== "") |> String.concat(seperator)

let wrap = (left, str, right) => str === "" ? "" : left ++ (str ++ right)

let indent = x =>
  switch x {
  | "" => ""
  | str => "  " ++ str->Js.String2.replaceByRe(%bs.re("/\\n/g"), "\n  ")
  }

let block = x =>
  switch x {
  | list{} => ""
  | list => "{\n" ++ (indent(join(list, "\n")) ++ "\n}")
  }

let rec printValue: value => string = x =>
  switch x {
  | #Int(int) => Js.Int.toString(int)
  | #Float(float) => Js.Float.toString(float)
  | #String(string) => string->Js.Json.string->Js.Json.stringify
  | #Boolean(bool) => string_of_bool(bool)
  | #Null => "null"
  | #Variable(string) => "$" ++ string
  | #Enum(enum) => enum
  | #List(values) => "[" ++ (join(values->List.map(printValue), ", ") ++ "]")
  | #Object(fields) => "{" ++ (printObjectFields(fields) ++ "}")
  }

and printObjectFields = fields => fields->List.map(printObjectField)->join(", ")

and printObjectField = ((k, v)) => k ++ (": " ++ printValue(v))

let rec printType = x =>
  switch x {
  | NamedType(string) => string
  | ListType(typ) => "[" ++ (printType(typ) ++ "]")
  | NonNullType(typ) => printType(typ) ++ "!"
  }

let printVariableDef = ({variable, typ}: variableDefinition) =>
  printValue((variable :> value)) ++ (": " ++ printType(typ))
let printVariables = vars => vars->List.map(printVariableDef)->join(", ")

let printArgument = ((name, value)) => name ++ (": " ++ printValue(value))
let printArguments = (args: list<(string, value)>) => args->List.map(printArgument)->join(", ")

let printDirective = ({name, arguments}: directive) =>
  "@" ++ (name ++ wrap("(", printArguments(arguments), ")"))

let printDirectives = directives => directives->List.map(printDirective)->join(" ")

let printOpt = x =>
  switch x {
  | Some(v) => v
  | None => ""
  }

let rec printSelectionSet = selectionSet => selectionSet->List.map(printSelection)->block

and printSelection = x =>
  switch x {
  | Field(field) => printField(field)
  | FragmentSpread(fragmentSpread) => printFragmentSpread(fragmentSpread)
  | InlineFragment(inlineFragmentDefinition) =>
    printInlineFragmentDefinition(inlineFragmentDefinition)
  }

and printField = ({alias, name, arguments, directives, selectionSet}) =>
  join(
    list{
      printAlias(alias) ++ (name ++ wrap("(", printArguments(arguments), ")")),
      printDirectives(directives),
      printSelectionSet(selectionSet),
    },
    " ",
  )

and printAlias = x =>
  switch x {
  | Some(alias) => alias ++ ": "
  | None => ""
  }

and printFragmentSpread = ({name, directives}) =>
  "..." ++ (name ++ wrap(" ", printDirectives(directives), ""))

and printInlineFragmentDefinition = ({typeCondition, selectionSet, directives}) =>
  join(
    list{
      "...",
      switch typeCondition {
      | Some(condition) => wrap("on ", condition, "")
      | None => ""
      },
      printDirectives(directives),
      printSelectionSet(selectionSet),
    },
    " ",
  )

let printOperationDef = (
  {operationType, variableDefinition, directives, selectionSet} as operationDef,
) => {
  let operationTypeStr = switch operationType {
  | Query => "query"
  | Subscription => "subscription"
  | Mutation => "mutation"
  }
  let varDefs = "(" ++ (printVariables(variableDefinition) ++ ")")
  let directives = printDirectives(directives)
  let selectionSet = printSelectionSet(selectionSet)

  switch operationDef {
  | {
      operationType: Query,
      name: None,
      directives: list{},
      variableDefinition: list{},
    } => selectionSet
  | {name} =>
    join(
      list{operationTypeStr, join(list{printOpt(name), varDefs}, ""), directives, selectionSet},
      " ",
    )
  }
}

let printFragmentDef = ({name, typeCondition, directives, selectionSet}) =>
  "fragment " ++
  (name ++
  (" on " ++
  (typeCondition ++
  (" " ++ (wrap("", printDirectives(directives), " ") ++ printSelectionSet(selectionSet))))))

let printDefinition = definition =>
  switch definition {
  | Operation(operationDef) => printOperationDef(operationDef)
  | Fragment(fragmentDef) => printFragmentDef(fragmentDef)
  }

let print = ({definitions}: document) => definitions->List.map(printDefinition)->join("\n\n")

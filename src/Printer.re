open Ast;
open Belt;

let join = (list, seperator) => list->List.keep(x => x !== "") |> String.concat(seperator);

let wrap = (left, str, right) => str === "" ? "" : left ++ str ++ right;

let indent =
  fun
  | "" => ""
  | str => "  " ++ (str |> Js.String.replaceByRe([%bs.re "/\\n/g"], "\n  "));

let block =
  fun
  | [] => ""
  | list => "{\n" ++ indent(join(list, "\n")) ++ "\n}";

let rec printValue =
  fun
  | Int(int) => string_of_int(int)
  | Float(float) => string_of_float(float)
  | String(string) => string
  | Boolean(bool) => string_of_bool(bool)
  | Null => "null"
  | Variable(string) => "$" ++ string
  | Enum(enum) => enum
  | List(values) => "[" ++ join(values->List.map(printValue), ", ") ++ "]"
  | Object(fields) => "{" ++ printObjectFields(fields) ++ "}"
and printObjectFields = fields =>
  fields->Ast.StringMap.bindings->List.map(printObjectField)->join(", ")
and printObjectField = ((k, v)) => k ++ ":" ++ printValue(v);

let rec printType =
  fun
  | NamedType(string) => string
  | ListType(typ) => "[" ++ printType(typ) ++ "]"
  | NonNullType(typ) => printType(typ) ++ "!";

let printVariableDef = var => var.variable->printValue;
let printVariables = vars =>
  vars
  ->List.map(varDef => printValue(varDef.variable) ++ ": " ++ printType(varDef.typ))
  ->join(", ");

let printArgument = (arg: argument) => arg.name ++ ": " ++ printValue(arg.value);
let printArguments = (args: list(argument)) => args->List.map(printArgument)->join(", ");

let printDirective = ({name, arguments}: directive) =>
  "@" ++ name ++ wrap("(", printArguments(arguments), ")");
let printDirectives = directives => directives->List.map(printDirective)->join(" ");

let printOpt =
  fun
  | Some(v) => v
  | None => "";

let rec printSelectionSet = (set: list(selection)) => set->List.map(printSelection)->block
and printSelection =
  fun
  | Field(field) => printField(field)
  | FragmentSpread(fragmentSpread) => printFragmentSpread(fragmentSpread)
  | InlineFragment(inlineFragmentDefinition) =>
    printInlineFragmentDefinition(inlineFragmentDefinition)
and printField = ({alias, name, arguments, directives, selectionSet}) =>
  join(
    [
      printAlias(alias) ++ name ++ wrap("(", printArguments(arguments), ")"),
      printDirectives(directives),
      printSelectionSet(selectionSet),
    ],
    " ",
  )
and printAlias =
  fun
  | Some(alias) => alias ++ ": "
  | None => ""
and printFragmentSpread = ({name, directives}) =>
  "..." ++ name ++ wrap(" ", printDirectives(directives), "")
and printInlineFragmentDefinition = ({typeCondition, selectionSet, directives}) =>
  join(
    [
      "...",
      wrap("on ", typeCondition, ""),
      printDirectives(directives),
      printSelectionSet(selectionSet),
    ],
    " ",
  );

let printOperationDef = (operationDef: operationDefinition) => {
  let op =
    switch (operationDef.operationType) {
    | Query => "query"
    | Subscription => "subscription"
    | Mutation => "mutation"
    };
  let varDefs = "(" ++ printVariables(operationDef.variableDefinition) ++ ")";
  let directives = printDirectives(operationDef.directives);
  let selectionSet = printSelectionSet(operationDef.selectionSet);

  switch (operationDef) {
  | {name: None, directives: [], variableDefinition: [], operationType: Query} => selectionSet
  | {name} => join([op, join([printOpt(name), varDefs], ""), directives, selectionSet], " ")
  };
};

let printFragmentDef = ({name, typeCondition, directives, selectionSet}: fragmentDefinition) =>
  "fragment "
  ++ name
  ++ " on "
  ++ typeCondition
  ++ " "
  ++ wrap("", printDirectives(directives), " ")
  ++ printSelectionSet(selectionSet);

let printDefinition = definition =>
  switch (definition) {
  | OperationDefinition(operationDef) => printOperationDef(operationDef)
  | FragmentDefinition(fragmentDef) => printFragmentDef(fragmentDef)
  };
let print = ({definitions}: document) => join(definitions->List.map(printDefinition), "\n\n");
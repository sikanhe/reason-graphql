open GraphqlLanguageAst;

let join = (list, seperator) =>
  list |> Belt.List.keep(_, x => x !== "") |> String.concat(seperator);

let wrap = (left, str, right) => str === "" ? "" : left ++ str ++ right;

let indent =
  fun
  | "" => ""
  | str => "  " ++ (str |> Js.String.replaceByRe([%bs.re "/\\n/g"], "\n  "));

let block =
  fun
  | [] => ""
  | list => "{\n" ++ indent(join(list, "\n")) ++ "\n}";

let rec printValue: value => string =
  fun
  | `Int(int) => string_of_int(int)
  | `Float(float) => string_of_float(float)
  | `String(string) => string
  | `Boolean(bool) => string_of_bool(bool)
  | `Null => "null"
  | `Variable(string) => "$" ++ string
  | `Enum(enum) => enum
  | `List(values) => "[" ++ join(values |> Belt.List.map(_, printValue), ", ") ++ "]"
  | `Map(fields) => "{" ++ printObjectFields(fields) ++ "}"

and printObjectFields = fields => fields |> Belt.List.map(_, printObjectField) |> join(_, ", ")

and printObjectField = ((k, v)) => k ++ ":" ++ printValue(v);

let rec printType =
  fun
  | NamedType(string) => string
  | ListType(typ) => "[" ++ printType(typ) ++ "]"
  | NonNullType(typ) => printType(typ) ++ "!";

let printVariableDef = ({variable, typ}: variableDefinition) =>
  printValue(variable) ++ ": " ++ printType(typ);
let printVariables = vars => vars |> Belt.List.map(_, printVariableDef) |> join(_, ", ");

let printArgument = ((name, value)) => name ++ ": " ++ printValue(value);
let printArguments = (args: list((string, value))) =>
  args |> Belt.List.map(_, printArgument) |> join(_, ", ");

let printDirective = ({name, arguments}: directive) =>
  "@" ++ name ++ wrap("(", printArguments(arguments), ")");

let printDirectives = directives =>
  directives |> Belt.List.map(_, printDirective) |> join(_, " ");

let printOpt =
  fun
  | Some(v) => v
  | None => "";

let rec printSelectionSet = selectionSet =>
  selectionSet |> Belt.List.map(_, printSelection) |> block

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

let printOperationDef =
    ({operationType, variableDefinition, directives, selectionSet} as operationDef) => {
  let operationTypeStr =
    switch (operationType) {
    | Query => "query"
    | Subscription => "subscription"
    | Mutation => "mutation"
    };
  let varDefs = "(" ++ printVariables(variableDefinition) ++ ")";
  let directives = printDirectives(directives);
  let selectionSet = printSelectionSet(selectionSet);

  switch (operationDef) {
  | {operationType: Query, name: None, directives: [], variableDefinition: []} => selectionSet
  | {name} =>
    join(
      [operationTypeStr, join([printOpt(name), varDefs], ""), directives, selectionSet],
      " ",
    )
  };
};

let printFragmentDef = ({name, typeCondition, directives, selectionSet}) =>
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

let print = ({definitions}: document) =>
  definitions |> Belt.List.map(_, printDefinition) |> join(_, "\n\n");
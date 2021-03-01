type primitiveValue = [
  | #Int(int)
  | #Float(float)
  | #Boolean(bool)
  | #String(string)
  | #Enum(string)
  | #Null
]

type rec constValue = [
  | primitiveValue
  | #List(list<constValue>)
  | #Object(list<(string, constValue)>)
]

type rec value = [
  | primitiveValue
  | #List(list<value>)
  | #Object(list<(string, value)>)
  | #Variable(string)
]

type argument = (string, value)

type rec document = {definitions: list<definition>}

and definition =
  | Operation(operationDefinition)
  | Fragment(fragmentDefinition)

and operationDefinition = {
  operationType: operationType,
  name: option<string>,
  variableDefinition: list<variableDefinition>,
  directives: list<directive>,
  selectionSet: list<selection>,
}

and operationType =
  | Query
  | Mutation
  | Subscription

and variableDefinition = {
  variable: [#Variable(string)],
  typ: typeReference,
  defaultValue: option<constValue>,
  directives: list<directive>,
}

and selection =
  | Field(field)
  | FragmentSpread(fragmentSpread)
  | InlineFragment(inlineFragmentDefinition)

and field = {
  alias: option<string>,
  name: string,
  arguments: list<argument>,
  selectionSet: list<selection>,
  directives: list<directive>,
}

and fragmentDefinition = {
  name: string,
  typeCondition: string,
  selectionSet: list<selection>,
  directives: list<directive>,
}

and inlineFragmentDefinition = {
  typeCondition: option<string>,
  selectionSet: list<selection>,
  directives: list<directive>,
}

and fragmentSpread = {
  name: string,
  directives: list<directive>,
}

/* Directives */
and directive = {
  name: string,
  arguments: list<argument>,
}

and typeReference =
  | NamedType(string)
  | ListType(typeReference)
  | NonNullType(typeReference)

and typeSystemDefinition =
  | SchemaDefinition(schemaDefinition)
  | TypeDefinition(typeDefinition)
  | TypeExtension(typeExtensionDefinition)
  | DirectiveDefinitionNode(directiveDefinition)

and schemaDefinition = {operationTypes: operationTypeDefinition}

and operationTypeDefinition = {
  typ: string,
  operation: operationType,
}

and typeDefinition =
  | ScalarTypeDefinition(string)
  | ObjectTypeDefinition(objectTypeDefinition)
  | InterfaceTypeDefinition(interfaceTypeDefinition)
  | UnionTypeDefinition(unionTypeDefinition)
  | EnumTypeDefinition(enumTypeDefintion)
  | InputObjectTypeDefinition(inputObjectTypeDefinition)

and objectTypeDefinition = {
  name: string,
  interfaces: list<string>,
  fields: list<fieldDefinition>,
}

and fieldDefinition = {
  name: string,
  arguments: list<inputValueDefinition>,
  typ: typeReference,
}

and inputValueDefinition = {
  name: string,
  typ: typeReference,
  defaultValue: option<constValue>,
}

and interfaceTypeDefinition = {
  name: string,
  fields: list<fieldDefinition>,
}

and unionTypeDefinition = {
  name: string,
  types: list<string>,
}

and enumTypeDefintion = {
  name: string,
  values: list<string>,
}

and inputObjectTypeDefinition = {
  name: string,
  fields: list<inputValueDefinition>,
}

and typeExtensionDefinition = {definition: objectTypeDefinition}

and directiveDefinition = {
  name: string,
  arguments: list<inputValueDefinition>,
}

type astMapping = {
  name: string => string,
  document: document => document,
  operationDefinition: operationDefinition => operationDefinition,
  fragmentDefinition: fragmentDefinition => fragmentDefinition,
  variableDefinition: variableDefinition => variableDefinition,
  directives: list<directive> => list<directive>,
  directive: directive => directive,
  selection: selection => selection,
  selectionSet: list<selection> => list<selection>,
  field: field => field,
  arguments: list<argument> => list<argument>,
  argument: argument => argument,
  inlineFragmentDefinition: inlineFragmentDefinition => inlineFragmentDefinition,
  fragmentSpread: fragmentSpread => fragmentSpread,
  typeReference: typeReference => typeReference,
  namedType: string => typeReference,
  listType: typeReference => typeReference,
  nonNullType: typeReference => typeReference,
  constValue: constValue => constValue,
  value: value => value,
  variable: string => string,
}

let id = a => a

let defaultMapper = {
  name: id,
  document: id,
  operationDefinition: id,
  fragmentDefinition: id,
  variableDefinition: id,
  directives: id,
  directive: id,
  selection: id,
  selectionSet: id,
  field: id,
  arguments: id,
  argument: id,
  inlineFragmentDefinition: id,
  fragmentSpread: id,
  typeReference: a => a,
  namedType: a => NamedType(a),
  constValue: id,
  value: id,
  listType: a => ListType(a),
  nonNullType: a => NonNullType(a),
  variable: id,
}

let rec visit = (~enter=defaultMapper, ~leave=defaultMapper, ast: document) => {
  let document = enter.document(ast)
  {
    definitions: document.definitions |> List.map(visitDefinition(~enter, ~leave)),
  } |> leave.document
}
and visitDefinition = (~enter, ~leave, x) =>
  switch x {
  | Operation(operationDefinition) =>
    Operation(visitOperationDefinition(~enter, ~leave, operationDefinition))
  | Fragment(fragmentDefinition) =>
    Fragment(visitFragmentDefinition(~enter, ~leave, fragmentDefinition))
  }
and visitOperationDefinition = (~enter, ~leave, operationDefinition) => {
  let operationDefinition = enter.operationDefinition(operationDefinition)
  {
    ...operationDefinition,
    name: switch operationDefinition.name {
    | Some(name) => Some(visitName(~enter, ~leave, name))
    | None => None
    },
    variableDefinition: List.map(
      visitVariableDefinition(~enter, ~leave),
      operationDefinition.variableDefinition,
    ),
    directives: visitDirectives(~enter, ~leave, operationDefinition.directives),
    selectionSet: visitSelectionSet(~enter, ~leave, operationDefinition.selectionSet),
  }
}
and visitFragmentDefinition = (~enter, ~leave, fragmentDefinition) => {
  let fragmentDefinition = enter.fragmentDefinition(fragmentDefinition)

  {
    ...fragmentDefinition,
    name: visitName(~enter, ~leave, fragmentDefinition.name),
    selectionSet: visitSelectionSet(~enter, ~leave, fragmentDefinition.selectionSet),
    directives: visitDirectives(~enter, ~leave, fragmentDefinition.directives),
  } |> leave.fragmentDefinition
}
and visitVariableDefinition = (~enter, ~leave, variableDefinition) => {
  let variableDefinition = enter.variableDefinition(variableDefinition)
  let #Variable(variable) = variableDefinition.variable

  {
    variable: #Variable(enter.variable(variable)),
    typ: visitTypeReference(~enter, ~leave, variableDefinition.typ),
    directives: visitDirectives(~enter, ~leave, variableDefinition.directives),
    defaultValue: switch variableDefinition.defaultValue {
    | Some(constValue) => Some(visitConstValue(~enter, ~leave, constValue))
    | None => None
    },
  } |> leave.variableDefinition
}
and visitDirectives = (~enter, ~leave, directives) =>
  directives |> enter.directives |> List.map(visitDirective(~enter, ~leave)) |> leave.directives
and visitDirective = (~enter, ~leave, directive) => {
  let directive = enter.directive(directive)
  {
    name: visitName(~enter, ~leave, directive.name),
    arguments: visitArguments(~enter, ~leave, directive.arguments),
  } |> leave.directive
}
and visitArguments = (~enter, ~leave, arguments) =>
  arguments |> enter.arguments |> List.map(visitArgument(~enter, ~leave)) |> leave.arguments
and visitArgument = (~enter, ~leave, argument) => {
  let (name, value) = enter.argument(argument)
  (name, visitValue(~enter, ~leave, value)) |> leave.argument
}
and visitSelectionSet = (~enter, ~leave, selectionSet) =>
  selectionSet
  |> enter.selectionSet
  |> List.map(visitSelection(~enter, ~leave))
  |> leave.selectionSet
and visitSelection = (~enter, ~leave, selection) =>
  switch enter.selection(selection) {
  | Field(field) => Field(visitField(~enter, ~leave, field))
  | FragmentSpread(fragmentSpread) =>
    FragmentSpread(visitFragmentSpread(~enter, ~leave, fragmentSpread))
  | InlineFragment(inlineFragment) =>
    InlineFragment(visitInlineFragment(~enter, ~leave, inlineFragment))
  } |> leave.selection
and visitField = (~enter, ~leave, field) => {
  let field = enter.field(field)
  {
    ...field,
    name: visitName(~enter, ~leave, field.name),
    arguments: visitArguments(~enter, ~leave, field.arguments),
    selectionSet: visitSelectionSet(~enter, ~leave, field.selectionSet),
    directives: visitDirectives(~enter, ~leave, field.directives),
  } |> leave.field
}
and visitFragmentSpread = (~enter, ~leave, fragmentSpread) => {
  let fragmentSpread = enter.fragmentSpread(fragmentSpread)
  {
    name: visitName(~enter, ~leave, fragmentSpread.name),
    directives: visitDirectives(~enter, ~leave, fragmentSpread.directives),
  } |> leave.fragmentSpread
}
and visitInlineFragment = (~enter, ~leave, inlineFragment) => {
  let inlineFragment = enter.inlineFragmentDefinition(inlineFragment)
  {
    ...inlineFragment,
    selectionSet: visitSelectionSet(~enter, ~leave, inlineFragment.selectionSet),
    directives: visitDirectives(~enter, ~leave, inlineFragment.directives),
  } |> leave.inlineFragmentDefinition
}
and visitTypeReference = (~enter, ~leave, typeReference) =>
  switch enter.typeReference(typeReference) {
  | NamedType(string) => enter.namedType(string)
  | ListType(listType) => enter.listType(listType)
  | NonNullType(nonNullType) => enter.nonNullType(nonNullType)
  } |> leave.typeReference
and visitName = (~enter, ~leave, name) => enter.name(name) |> leave.name
and visitValue = (~enter, ~leave, value) => enter.value(value) |> leave.value
and visitConstValue = (~enter, ~leave, constValue) =>
  enter.constValue(constValue) |> leave.constValue

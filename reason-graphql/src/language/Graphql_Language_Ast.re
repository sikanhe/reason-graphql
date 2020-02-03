type primitiveValue = [
  | `Int(int)
  | `Float(float)
  | `Boolean(bool)
  | `String(string)
  | `Enum(string)
  | `Null
];

type constValue = [
  primitiveValue
  | `List(list(constValue))
  | `Map(list((string, constValue)))
];

type value = [
  primitiveValue
  | `List(list(value))
  | `Map(list((string, value)))
  | `Variable(string)
];

type document = {definitions: list(definition)}

and definition =
  | OperationDefinition(operationDefinition)
  | FragmentDefinition(fragmentDefinition)

and operationDefinition = {
  operationType,
  name: option(string),
  variableDefinition: list(variableDefinition),
  directives: list(directive),
  selectionSet: list(selection),
}

and operationType =
  | Query
  | Mutation
  | Subscription

and variableDefinition = {
  variable: [ | `Variable(string)],
  typ: typeReference,
  defaultValue: option(constValue),
  directives: list(directive),
}

and selection =
  | Field(field)
  | FragmentSpread(fragmentSpread)
  | InlineFragment(inlineFragmentDefinition)

and field = {
  alias: option(string),
  name: string,
  arguments: list((string, value)),
  selectionSet: list(selection),
  directives: list(directive),
}

and fragmentDefinition = {
  name: string,
  typeCondition: string,
  selectionSet: list(selection),
  directives: list(directive),
}

and inlineFragmentDefinition = {
  typeCondition: option(string),
  selectionSet: list(selection),
  directives: list(directive),
}

and fragmentSpread = {
  name: string,
  directives: list(directive),
}

/* Directives */
and directive = {
  name: string,
  arguments: list((string, value)),
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
  interfaces: list(string),
  fields: list(fieldDefinition),
}

and fieldDefinition = {
  name: string,
  arguments: list(inputValueDefinition),
  typ: typeReference,
}

and inputValueDefinition = {
  name: string,
  typ: typeReference,
  defaultValue: option(constValue),
}

and interfaceTypeDefinition = {
  name: string,
  fields: list(fieldDefinition),
}

and unionTypeDefinition = {
  name: string,
  types: list(string),
}

and enumTypeDefintion = {
  name: string,
  values: list(string),
}

and inputObjectTypeDefinition = {
  name: string,
  fields: list(inputValueDefinition),
}

and typeExtensionDefinition = {definition: objectTypeDefinition}

and directiveDefinition = {
  name: string,
  arguments: list(inputValueDefinition),
};

type astMapping = {
  name: string => string,
  document: document => document,
  operationDefinition: operationDefinition => operationDefinition,
  fragmentDefinition: fragmentDefinition => fragmentDefinition,
  variableDefinition: variableDefinition => variableDefinition,
  directives: list(directive) => list(directive),
  directive: directive => directive,
  selection: selection => selection,
  selectionSet: list(selection) => list(selection),
  field: field => field,
  arguments: list((string, value)) => list((string, value)),
  argument: ((string, value)) => (string, value),
  inlineFragmentDefinition:
    inlineFragmentDefinition => inlineFragmentDefinition,
  fragmentSpread: fragmentSpread => fragmentSpread,
  typeReference: typeReference => typeReference,
  namedType: string => typeReference,
  listType: typeReference => typeReference,
  nonNullType: typeReference => typeReference,
  constValue: constValue => constValue,
  value: value => value,
  variable: string => string,
};

let id = a => a;

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
};

let rec visit = (mapper, ast: document) => {
  let document = mapper.document(ast);
  {definitions: document.definitions |> List.map(visitDefinition(mapper))};
}
and visitDefinition = mapper =>
  fun
  | OperationDefinition(node) =>
    OperationDefinition(visitOperationDefinition(mapper, node))
  | FragmentDefinition(node) =>
    FragmentDefinition(visitFragmentDefinition(mapper, node))
and visitOperationDefinition = (mapper, node) => {
  let node = mapper.operationDefinition(node);
  {
    ...node,
    name:
      switch (node.name) {
      | Some(name) => Some(visitName(mapper, name))
      | None => None
      },
    variableDefinition:
      List.map(visitVariableDefinition(mapper), node.variableDefinition),
    directives: visitDirectives(mapper, node.directives),
  };
}
and visitFragmentDefinition = (mapper, node) => {
  let node = mapper.fragmentDefinition(node);

  {
    ...node,
    name: visitName(mapper, node.name),
    selectionSet: visitSelectionSet(mapper, node.selectionSet),
    directives: visitDirectives(mapper, node.directives),
  };
}
and visitVariableDefinition = (mapper, node) => {
  let variableDefinition = mapper.variableDefinition(node);
  let `Variable(variable) = variableDefinition.variable;

  {
    variable: `Variable(mapper.variable(variable)),
    typ: visitTypeReference(mapper, variableDefinition.typ),
    directives: visitDirectives(mapper, variableDefinition.directives),
    defaultValue:
      switch (variableDefinition.defaultValue) {
      | Some(constValue) => Some(visitConstValue(mapper, constValue))
      | None => None
      },
  };
}
and visitDirectives = (mapper, directives) => {
  let directives = mapper.directives(directives);
  List.map(visitDirective(mapper), directives);
}
and visitDirective = (mapper, directive) => {
  let directive = mapper.directive(directive);
  {
    name: visitName(mapper, directive.name),
    arguments: visitArguments(mapper, directive.arguments),
  };
}
and visitArguments = (mapper, arguments) => {
  arguments |> mapper.arguments |> List.map(visitArgument(mapper));
}
and visitArgument = (mapper, argument) => {
  let (name, value) = mapper.argument(argument);
  (name, visitValue(mapper, value));
}
and visitSelectionSet = (mapper, selectionSet) => {
  selectionSet |> mapper.selectionSet |> List.map(visitSelection(mapper));
}
and visitSelection = (mapper, selection) => {
  switch (mapper.selection(selection)) {
  | Field(field) => Field(visitField(mapper, field))
  | FragmentSpread(fragmentSpread) =>
    FragmentSpread(visitFragmentSpread(mapper, fragmentSpread))
  | InlineFragment(inlineFragment) =>
    InlineFragment(visitInlineFragment(mapper, inlineFragment))
  };
}
and visitField = (mapper, field) => {
  let field = mapper.field(field);

  {
    ...field,
    name: visitName(mapper, field.name),
    arguments: visitArguments(mapper, field.arguments),
    selectionSet: visitSelectionSet(mapper, field.selectionSet),
    directives: visitDirectives(mapper, field.directives),
  };
}
and visitFragmentSpread = (mapper, fragmentSpread) => {
  let fragmentSpread = mapper.fragmentSpread(fragmentSpread);
  {
    name: visitName(mapper, fragmentSpread.name),
    directives: visitDirectives(mapper, fragmentSpread.directives),
  };
}
and visitInlineFragment = (mapper, inlineFragment) => {
  let inlineFragment = mapper.inlineFragmentDefinition(inlineFragment);
  {
    ...inlineFragment,
    selectionSet: visitSelectionSet(mapper, inlineFragment.selectionSet),
    directives: visitDirectives(mapper, inlineFragment.directives),
  };
}
and visitTypeReference = (mapper, typeReference) => {
  let typeReference = mapper.typeReference(typeReference);
  switch (typeReference) {
  | NamedType(string) => mapper.namedType(string)
  | ListType(listType) => mapper.listType(listType)
  | NonNullType(nonNullType) => mapper.nonNullType(nonNullType)
  };
}
and visitName = (mapper, name) => {
  mapper.name(name);
}
and visitValue = (mapper, value) => {
  mapper.value(value);
}
and visitConstValue = (mapper, constValue) => {
  mapper.constValue(constValue);
};
module StringMap = Map.Make(String);

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
  variable: value,
  typ: typeReference,
  defaultValue: option(value),
  directives: list(directive),
}
and selection =
  | Field(field)
  | FragmentSpread(fragmentSpread)
  | InlineFragment(inlineFragmentDefinition)
and field = {
  alias: option(string),
  name: string,
  arguments: list(argument),
  selectionSet: list(selection),
  directives: list(directive),
}
and argument = {
  name: string,
  value,
}
/* Fragments */
and fragmentDefinition = {
  name: string,
  typeCondition: string,
  selectionSet: list(selection),
  directives: list(directive),
}
and inlineFragmentDefinition = {
  typeCondition: string,
  selectionSet: list(selection),
  directives: list(directive),
}
and fragmentSpread = {
  name: string,
  directives: list(directive),
}
/* Values */
and value =
  | Int(int)
  | Float(float)
  | Boolean(bool)
  | String(string)
  | Enum(string)
  | List(list(value))
  | Object(StringMap.t(value))
  | Variable(string)
  | Null
/* Directives */
and directive = {
  name: string,
  arguments: list(argument),
}
/* Type Reference */
and typeReference =
  | NamedType(string)
  | ListType(typeReference)
  | NonNullType(typeReference)
/* Type System Definition */
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
  defaultValue: option(value),
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
open Ast;

/**
 * Determines if the next token is of a given kind
 */
let peek = (lexer: Lexer.t, kind: Lexer.tokenKind): bool => lexer.token.kind == kind;

/**
 * If the next token is of the given kind, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and throw an error.
 */
let expect = (lexer: Lexer.t, kind: Lexer.tokenKind) => {
  let token = lexer.token;
  if (token.kind == kind) {
    Lexer.advance(lexer)->ignore;
    token;
  } else {
    Js.log(Lexer.strOfTokenKind(kind));
    failwith("Expected" ++ Lexer.strOfTokenKind(kind) ++ ", found " ++ Lexer.printToken(token));
  };
};

/**
 * If the next token is of the given kind, return true after advancing
 * the lexer. Otherwise, do not change the parser state and return false.
 */
let skip = (lexer: Lexer.t, kind: Lexer.tokenKind): bool =>
  if (lexer.token.kind == kind) {
    Lexer.advance(lexer)->ignore;
    true;
  } else {
    false;
  };

/**
 * If the next token is a keyword with the given value, return true after advancing
 * the lexer. Otherwise, do not change the parser state and return false.
 */
let skipKeyword = (lexer: Lexer.t, value: string): bool =>
  switch (lexer.token) {
  | {kind, value: v} when kind == NAME && v == value =>
    lexer->Lexer.advance->ignore;
    true;
  | _ => false
  };

/**
 * If the next token is a keyword with the given value, return that token after
 * advancing the lexer. Otherwise, do not change the parser state and throw
 * an error.
 */
let expectKeyword = (lexer: Lexer.t, value: string) =>
  if (!skipKeyword(lexer, value)) {
    failwith("Expected " ++ value ++ ", found " ++ Lexer.printToken(lexer.token));
  };

/**
 * Helper function for creating an error when an unexpected lexed token
 * is encountered.
 */
let unexpected = (~atToken=?, lexer: Lexer.t) => {
  let token =
    switch (atToken) {
    | Some(tok) => tok
    | None => lexer.token
    };
  failwith("Unexpected " ++ Lexer.printToken(token));
};

/**
 * Returns a possibly empty list of parse nodes, determined by
 * the parseFn. This list begins with a lex token of openKind
 * and ends with a lex token of closeKind. Advances the parser
 * to the next lex token after the closing token.
 */
let any =
    (
      lexer: Lexer.t,
      openKind: Lexer.tokenKind,
      parseFn: Lexer.t => 'a,
      closeKind: Lexer.tokenKind,
    ) => {
  expect(lexer, openKind)->ignore;
  let nodes = ref([]);
  while (!skip(lexer, closeKind)) {
    nodes := [parseFn(lexer), ...nodes^];
  };
  List.rev(nodes^);
};

/**
 * Returns a non-empty list of parse nodes, determined by
 * the parseFn. This list begins with a lex token of openKind
 * and ends with a lex token of closeKind. Advances the parser
 * to the next lex token after the closing token.
 */
let many =
    (
      lexer: Lexer.t,
      openKind: Lexer.tokenKind,
      parseFn: Lexer.t => 'a,
      closeKind: Lexer.tokenKind,
    ) => {
  expect(lexer, openKind)->ignore;
  let nodes = ref([parseFn(lexer)]);
  while (!skip(lexer, closeKind)) {
    nodes := [parseFn(lexer), ...nodes^];
  };
  List.rev(nodes^);
};

let parseStringLiteral = ({token} as lexer: Lexer.t) => {
  Lexer.advance(lexer)->ignore;
  String(token.value);
};

let parseName = (lexer: Lexer.t) => {
  let token = expect(lexer, NAME);
  token.value;
};

let parseNamedType = (lexer: Lexer.t) => NamedType(parseName(lexer));

/**
 * Variable : $ Name
 */
let parseVariable = (lexer: Lexer.t) => {
  expect(lexer, DOLLAR)->ignore;
  Variable(parseName(lexer));
};

/**
 * Value[Const] :
 *   - [~Const] Variable
 *   - IntValue
 *   - FloatValue
 *   - StringValue
 *   - BooleanValue
 *   - NullValue
 *   - EnumValue
 *   - ListValue[?Const]
 *   - ObjectValue[?Const]
 *
 * BooleanValue : one of `true` `false`
 *
 * NullValue : `null`
 *
 * EnumValue : Name but not `true`, `false` or `null`
 */
let rec parseValueLiteral = ({token} as lexer: Lexer.t, ~isConst: bool) =>
  switch (token.kind) {
  | BRACKET_L => parseList(lexer, ~isConst)
  | BRACE_L => parseObject(lexer, ~isConst)
  | INT =>
    Lexer.advance(lexer)->ignore;
    Int(int_of_string(token.value));
  | FLOAT =>
    Lexer.advance(lexer)->ignore;
    Float(float_of_string(token.value));
  | STRING => parseStringLiteral(lexer)
  | NAME =>
    switch (token.value) {
    | "true" =>
      Lexer.advance(lexer)->ignore;
      Boolean(true);
    | "false" =>
      Lexer.advance(lexer)->ignore;
      Boolean(false);
    | "null" =>
      Lexer.advance(lexer)->ignore;
      Null;
    | enum =>
      Lexer.advance(lexer)->ignore;
      Enum(enum);
    }
  | DOLLAR when !isConst => parseVariable(lexer)
  | _ => unexpected(lexer)
  }
/**
 * ListValue[Const] :
 *   - [ ]
 *   - [ Value[?Const]+ ]
 */
and parseList = (lexer: Lexer.t, ~isConst: bool) => {
  let parseFn = parseValueLiteral(~isConst);
  List(any(lexer, BRACKET_L, parseFn, BRACKET_R));
}
/**
 * ObjectValue[Const] :
 *   - { }
 *   - { ObjectField[?Const]+ }
 */
and parseObject = (lexer: Lexer.t, ~isConst: bool) => {
  expect(lexer, BRACE_L)->ignore;
  let fields = ref(StringMap.empty);
  while (!skip(lexer, BRACE_R)) {
    let (name, value) = parseObjectField(lexer, ~isConst);
    fields := fields^ |> StringMap.add(name, value);
  };
  Object(fields^);
}
/**
 * ObjectField[Const] : Name : Value[?Const]
 */
and parseObjectField = (lexer: Lexer.t, ~isConst: bool) => {
  let name = parseName(lexer);
  expect(lexer, COLON)->ignore;
  let value = parseValueLiteral(lexer, ~isConst);
  (name, value);
};

let rec parseTypeReference = (lexer: Lexer.t) => {
  let typ =
    if (skip(lexer, BRACKET_L)) {
      let t = parseTypeReference(lexer);
      expect(lexer, BRACKET_R)->ignore;
      ListType(t);
    } else {
      parseNamedType(lexer);
    };

  skip(lexer, BANG) ? NonNullType(typ) : typ;
};

let parseArgument = (lexer: Lexer.t): argument => {
  let name = parseName(lexer);
  expect(lexer, COLON)->ignore;
  {name, value: parseValueLiteral(lexer, ~isConst=false)};
};

let parseConstArgument = (lexer: Lexer.t): argument => {
  let name = parseName(lexer);
  expect(lexer, COLON)->ignore;
  {name, value: parseValueLiteral(lexer, ~isConst=true)};
};

let parseArguments = (lexer: Lexer.t, ~isConst: bool) => {
  let parseFn = isConst ? parseConstArgument : parseArgument;
  peek(lexer, PAREN_L) ? many(lexer, PAREN_L, parseFn, PAREN_R) : [];
};

let parseDirective = (lexer: Lexer.t, ~isConst: bool): directive => {
  expect(lexer, AT)->ignore;
  {name: parseName(lexer), arguments: parseArguments(lexer, ~isConst)};
};

let parseDirectives = (lexer: Lexer.t, ~isConst: bool) => {
  let directives = ref([]);
  while (peek(lexer, AT)) {
    directives := [parseDirective(lexer, ~isConst), ...directives^];
  };
  List.rev(directives^);
};

/* Operation Definitions */

let parseOperationType = (lexer: Lexer.t) => {
  let operationToken = expect(lexer, NAME);
  switch (operationToken.value) {
  | "query" => Query
  | "mutation" => Mutation
  | "subscription" => Subscription
  | _ => unexpected(lexer, ~atToken=operationToken)
  };
};

let parseVariableDefinition = (lexer: Lexer.t): variableDefinition => {
  let variable = parseVariable(lexer);
  expect(lexer, COLON)->ignore;
  let typ = parseTypeReference(lexer);
  {typ, variable, defaultValue: None, directives: parseDirectives(lexer, ~isConst=true)};
};

let parseVariableDefinitions = (lexer: Lexer.t) =>
  peek(lexer, PAREN_L) ? many(lexer, PAREN_L, parseVariableDefinition, PAREN_R) : [];

/**
 * SelectionSet : { Selection+ }
 */
let rec parseSelectionSet = (lexer: Lexer.t): list(selection) =>
  many(lexer, BRACE_L, parseSelection, BRACE_R)
/**
 * Selection :
 *   - Field
 *   - FragmentSpread
 *   - InlineFragment
 */
and parseSelection = (lexer: Lexer.t): selection =>
  if (peek(lexer, SPREAD)) {
    parseFragment(lexer);
  } else {
    parseField(lexer);
  }
/**
 * FragmentName : Name but not `on`
 */
and parseFragmentName = ({token} as lexer: Lexer.t) =>
  if (token.value == "on") {
    unexpected(lexer);
  } else {
    parseName(lexer);
  }
/**
 * Corresponds to both FragmentSpread and InlineFragment in the spec.
 * FragmentSpread : ... FragmentName Directives?
 * InlineFragment : ... TypeCondition? Directives? SelectionSet
 */
and parseFragment = (lexer: Lexer.t) => {
  expect(lexer, SPREAD)->ignore;
  let hasTypeCondition = skipKeyword(lexer, "on");

  if (!hasTypeCondition && peek(lexer, NAME)) {
    FragmentSpread({
      name: parseFragmentName(lexer),
      directives: parseDirectives(lexer, ~isConst=false),
    });
  } else {
    InlineFragment({
      typeCondition: parseName(lexer),
      directives: parseDirectives(lexer, ~isConst=false),
      selectionSet: parseSelectionSet(lexer),
    });
  };
}
and parseField = (lexer: Lexer.t) => {
  let name = parseName(lexer);
  let (alias, name) =
    if (skip(lexer, COLON)) {
      (Some(name), parseName(lexer));
    } else {
      (None, name);
    };

  let arguments = parseArguments(lexer, ~isConst=false);
  let directives = parseDirectives(lexer, ~isConst=false);
  let selectionSet = peek(lexer, BRACE_L) ? parseSelectionSet(lexer) : [];

  Field({name, alias, arguments, directives, selectionSet});
};

/**
 * OperationDefinition :
 *  - SelectionSet
 *  - OperationType Name? VariableDefinitions? Directives? SelectionSet
 */
let parseOperationDefintiion = (lexer: Lexer.t) =>
  if (peek(lexer, BRACE_L)) {
    OperationDefinition({
      operationType: Query,
      name: None,
      variableDefinition: [],
      directives: [],
      selectionSet: parseSelectionSet(lexer),
    });
  } else {
    let operationType = parseOperationType(lexer);
    let name = peek(lexer, NAME) ? Some(parseName(lexer)) : None;
    let variableDefinition = parseVariableDefinitions(lexer);
    let directives = parseDirectives(lexer, ~isConst=false);
    let selectionSet = parseSelectionSet(lexer);

    OperationDefinition({operationType, name, variableDefinition, directives, selectionSet});
  };

/**
 * FragmentDefinition :
 *   - fragment FragmentName on TypeCondition Directives? SelectionSet
 *
 * TypeCondition : NamedType
 */
let parseFragmentDefinition = (lexer: Lexer.t) => {
  expectKeyword(lexer, "fragment");
  let name = parseFragmentName(lexer);
  expectKeyword(lexer, "on");

  let typeCondition = parseName(lexer);
  let selectionSet = parseSelectionSet(lexer);
  let directives = parseDirectives(lexer, ~isConst=false);

  FragmentDefinition({typeCondition, name, selectionSet, directives});
};

/**
 * ExecutableDefinition :
 *   - OperationDefinition
 *   - FragmentDefinition
 */
let parseExecutableDefinition = ({token} as lexer: Lexer.t) =>
  if (peek(lexer, NAME)) {
    switch (token.value) {
    | "query"
    | "mutation"
    | "subscription" => parseOperationDefintiion(lexer)
    | "fragment" => { parseFragmentDefinition(lexer)}
    | _ => unexpected(lexer)
    };
  } else if (peek(lexer, BRACE_L)) {
    parseOperationDefintiion(lexer);
  } else {
    unexpected(lexer);
  };

/**
 * Definition :
 *   - ExecutableDefinition
 *   - TypeSystemDefinition
 *   - TypeSystemExtension
 */
let parseDefinition = (lexer: Lexer.t) =>
  if (peek(lexer, NAME)) {
    switch (lexer.token.value) {
    | "query"
    | "mutation"
    | "subscription"
    | "fragment" => parseExecutableDefinition(lexer)
    | "schema"
    | "scalar"
    | "type"
    | "interface"
    | "union"
    | "enum"
    | "input"
    | "directive" => failwith("implement type system defintion")
    | "extend" => failwith("implmeent type system extension")
    | _ => unexpected(lexer)
    };
  } else if (peek(lexer, BRACE_L)) {
    parseExecutableDefinition(lexer);
  } else {
    failwith("implement");
  };

/**
 * Document : Definition+
 */
let parseDocument = (lexer: Lexer.t): document => {
  definitions: many(lexer, SOF, parseDefinition, EOF),
};

let parse = (body: string) => {
  let lexer = Lexer.make(body);
  parseDocument(lexer);
};
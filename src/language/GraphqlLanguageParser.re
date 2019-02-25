open GraphqlLanguageAst;
module Lexer = GraphqlLanguageLexer;

/**
 * If the next token is of the given kind, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and throw an error.
 */
let expect = (lexer: Lexer.t, kind: Lexer.tokenKind) =>
  if (lexer.token.kind == kind) {
    Lexer.advance(lexer);
  } else {
    failwith(
      "Expected" ++ Lexer.strOfTokenKind(kind) ++ ", found " ++ Lexer.printToken(lexer.token),
    );
  };

/**
 * If the next token is of the given kind, return true after advancing
 * the lexer. Otherwise, do not change the parser state and return false.
 */
let skip = (lexer: Lexer.t, kind: Lexer.tokenKind): bool =>
  if (lexer.token.kind == kind) {
    Lexer.advance(lexer);
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
    lexer |> Lexer.advance;
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
  expect(lexer, openKind);
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
  expect(lexer, openKind);
  let nodes = ref([parseFn(lexer)]);
  while (!skip(lexer, closeKind)) {
    nodes := [parseFn(lexer), ...nodes^];
  };
  List.rev(nodes^);
};

let parseStringLiteral = ({token} as lexer: Lexer.t) => {
  Lexer.advance(lexer);
  `String(token.value);
};

let parseName = (lexer: Lexer.t) => {
  let token = lexer.token;
  expect(lexer, NAME);
  token.value;
};

let parseNamedType = (lexer: Lexer.t) => NamedType(parseName(lexer));

/**
 * Variable : $ Name
 */
let parseVariable = (lexer: Lexer.t) => {
  expect(lexer, DOLLAR);
  `Variable(parseName(lexer));
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
    Lexer.advance(lexer);
    `Int(int_of_string(token.value));
  | FLOAT =>
    Lexer.advance(lexer);
    `Float(float_of_string(token.value));
  | STRING => parseStringLiteral(lexer)
  | NAME =>
    let value =
      switch (token.value) {
      | "true" => `Boolean(true)
      | "false" => `Boolean(false)
      | "null" => `Null
      | enum => `Enum(enum)
      };
    Lexer.advance(lexer);
    value;
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
  `List(any(lexer, BRACKET_L, parseFn, BRACKET_R));
}

/**
 * ObjectValue[Const] :
 *   - { }
 *   - { ObjectField[?Const]+ }
 */
and parseObject = (lexer: Lexer.t, ~isConst: bool) => {
  expect(lexer, BRACE_L);

  let rec makeFields = fields =>
    if (!skip(lexer, BRACE_R)) {
      let field = parseObjectField(lexer, ~isConst);
      makeFields([field, ...fields]);
    } else {
      fields;
    };

  `Map(makeFields([]));
}

/**
 * ObjectField[Const] : Name : Value[?Const]
 */
and parseObjectField = (lexer: Lexer.t, ~isConst: bool) => {
  let name = parseName(lexer);
  expect(lexer, COLON);
  let value = parseValueLiteral(lexer, ~isConst);
  (name, value);
};

let rec parseTypeReference = (lexer: Lexer.t) => {
  let typ =
    if (skip(lexer, BRACKET_L)) {
      let t = parseTypeReference(lexer);
      expect(lexer, BRACKET_R);
      ListType(t);
    } else {
      parseNamedType(lexer);
    };

  skip(lexer, BANG) ? NonNullType(typ) : typ;
};

let parseArgument = (lexer: Lexer.t): (string, value) => {
  let name = parseName(lexer);
  expect(lexer, COLON);
  (name, parseValueLiteral(lexer, ~isConst=false));
};

let parseConstArgument = (lexer: Lexer.t): (string, value) => {
  let name = parseName(lexer);
  expect(lexer, COLON);
  (name, parseValueLiteral(lexer, ~isConst=true));
};

let parseArguments = (lexer: Lexer.t, ~isConst: bool) =>
  switch (lexer.token.kind) {
  | PAREN_L when isConst => many(lexer, PAREN_L, parseConstArgument, PAREN_R)
  | PAREN_L => many(lexer, PAREN_L, parseArgument, PAREN_R)
  | _ => []
  };

let parseDirective = (lexer: Lexer.t, ~isConst: bool): directive => {
  expect(lexer, AT);
  {name: parseName(lexer), arguments: parseArguments(lexer, ~isConst)};
};

let parseDirectives = (lexer: Lexer.t, ~isConst: bool) => {
  let directives = ref([]);
  while (lexer.token.kind == AT) {
    directives := [parseDirective(lexer, ~isConst), ...directives^];
  };
  List.rev(directives^);
};

/* Operation Definitions */

let parseOperationType = (lexer: Lexer.t) => {
  let operationToken = lexer.token;
  expect(lexer, NAME);
  switch (operationToken.value) {
  | "query" => Query
  | "mutation" => Mutation
  | "subscription" => Subscription
  | _ => unexpected(lexer, ~atToken=operationToken)
  };
};

let parseVariableDefinition = (lexer: Lexer.t): variableDefinition => {
  let variable = parseVariable(lexer);
  expect(lexer, COLON);
  let typ = parseTypeReference(lexer);
  {typ, variable, defaultValue: None, directives: parseDirectives(lexer, ~isConst=true)};
};

let parseVariableDefinitions = (lexer: Lexer.t) =>
  switch (lexer.token.kind) {
  | PAREN_L => many(lexer, PAREN_L, parseVariableDefinition, PAREN_R)
  | _ => []
  };

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
  switch (lexer.token.kind) {
  | SPREAD => parseFragment(lexer)
  | _ => parseField(lexer)
  }

/**
 * FragmentName : Name but not `on`
 */
and parseFragmentName = ({token} as lexer: Lexer.t) =>
  switch (token.value) {
  | "on" => unexpected(lexer)
  | _ => parseName(lexer)
  }

/**
 * Corresponds to both FragmentSpread and InlineFragment in the spec.
 * FragmentSpread : ... FragmentName Directives?
 * InlineFragment : ... TypeCondition? Directives? SelectionSet
 */
and parseFragment = (lexer: Lexer.t) => {
  expect(lexer, SPREAD);
  let hasTypeCondition = skipKeyword(lexer, "on");

  switch (lexer.token.kind) {
  | NAME when !hasTypeCondition =>
    FragmentSpread({
      name: parseFragmentName(lexer),
      directives: parseDirectives(lexer, ~isConst=false),
    })
  | _ =>
    InlineFragment({
      typeCondition: parseName(lexer),
      directives: parseDirectives(lexer, ~isConst=false),
      selectionSet: parseSelectionSet(lexer),
    })
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
  let selectionSet =
    switch (lexer.token.kind) {
    | BRACE_L => parseSelectionSet(lexer)
    | _ => []
    };

  Field({name, alias, arguments, directives, selectionSet});
};

/**
 * OperationDefinition :
 *  - SelectionSet
 *  - OperationType Name? VariableDefinitions? Directives? SelectionSet
 */
let parseOperationDefinition = (lexer: Lexer.t) =>
  switch (lexer.token.kind) {
  | BRACE_L =>
    OperationDefinition({
      operationType: Query,
      name: None,
      variableDefinition: [],
      directives: [],
      selectionSet: parseSelectionSet(lexer),
    })
  | _ =>
    let operationType = parseOperationType(lexer);
    let name =
      switch (lexer.token.kind) {
      | NAME => Some(parseName(lexer))
      | _ => None
      };
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
  switch (token.kind) {
  | NAME =>
    switch (token.value) {
    | "query"
    | "mutation"
    | "subscription" => parseOperationDefinition(lexer)
    | "fragment" => parseFragmentDefinition(lexer)
    | _ => unexpected(lexer)
    }
  | BRACE_L => parseOperationDefinition(lexer)
  | _ => unexpected(lexer)
  };

/**
 * Definition :
 *   - ExecutableDefinition
 *   - TypeSystemDefinition
 *   - TypeSystemExtension
 */
let parseDefinition = (lexer: Lexer.t) =>
  switch (lexer.token.kind) {
  | NAME =>
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
    }
  | BRACE_L => parseExecutableDefinition(lexer)
  | _ => failwith("implement")
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
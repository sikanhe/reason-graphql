open GraphqlLanguageAst;
module Lexer = GraphqlLanguageLexer;
module Result = Belt.Result;

type result('a) = Result.t('a, GraphqlLanguageError.t);

let syntaxError = a => Result.Error(GraphqlLanguageError.SyntaxError(a));

/**
 * If the next token is of the given kind, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and throw an error.
 */
let expect = (lexer: Lexer.t, kind: Lexer.tokenKind) =>
  if (lexer.token.kind == kind) {
    Lexer.advance(lexer);
    Result.Ok();
  } else {
    syntaxError(
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
let expectKeyword = (lexer: Lexer.t, value: string, continuation: unit => result('a)) =>
  if (!skipKeyword(lexer, value)) {
    syntaxError("Expected " ++ value ++ ", found " ++ Lexer.printToken(lexer.token));
  } else {
    Ok();
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

  syntaxError("Unexpected " ++ Lexer.printToken(token));
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
      parseFn: Lexer.t => result('a),
      closeKind: Lexer.tokenKind,
    ) =>
  expect(lexer, openKind)
  ->Result.map(() => {
      let nodes = ref([]);
      let error = ref(None);

      while (error^ == None && !skip(lexer, closeKind)) {
        switch (parseFn(lexer)) {
        | Ok(value) => nodes := [value, ...nodes^]
        | err => error := Some(err)
        };
      };

      List.rev(nodes^);
    });

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
      parseFn: Lexer.t => result('a),
      closeKind: Lexer.tokenKind,
    )
    : result(list('a)) => {
  expect(lexer, openKind)
  ->Result.flatMap(() => parseFn(lexer))
  ->Result.flatMap(node => {
      let nodes = ref([node]);
      let error = ref(None);

      while (error^ != None && !skip(lexer, closeKind)) {
        switch (parseFn(lexer)) {
        | Ok(node) => nodes := [node, ...nodes^]
        | Error(err) => error := Some(Result.Error(err))
        };
      };

      switch (error^) {
      | Some(err) => err
      | None => Ok(Belt.List.reverse(nodes^))
      };
    });
};

let parseStringLiteral = ({token} as lexer: Lexer.t) => {
  Lexer.advance(lexer);
  `String(token.value);
};

let parseName = (lexer: Lexer.t) => {
  let token = lexer.token;
  expect(lexer, NAME)->Result.map(() => token.value);
};

let parseNamedType = (lexer: Lexer.t) => parseName(lexer)->Result.map(name => NamedType(name));

/**
 * Variable : $ Name
 */
let parseVariable = (lexer: Lexer.t) => {
  expect(lexer, DOLLAR)
  ->Result.flatMap(() => parseName(lexer)->Result.map(name => `Variable(name)));
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
let rec parseValueLiteral = ({token} as lexer: Lexer.t, ~isConst: bool): result(value) =>
  switch (token.kind) {
  | BRACKET_L => parseList(lexer, ~isConst)
  | BRACE_L => parseObject(lexer, ~isConst)
  | INT =>
    Lexer.advance(lexer);
    Result.Ok(`Int(int_of_string(token.value)));
  | FLOAT =>
    Lexer.advance(lexer);
    Result.Ok(`Float(float_of_string(token.value)));
  | STRING => Result.Ok(parseStringLiteral(lexer))
  | NAME =>
    let value =
      switch (token.value) {
      | "true" => `Boolean(true)
      | "false" => `Boolean(false)
      | "null" => `Null
      | enum => `Enum(enum)
      };
    Lexer.advance(lexer);
    Result.Ok(value);
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
  any(lexer, BRACKET_L, parseFn, BRACKET_R)->Result.map(list => `List(list));
}

/**
 * ObjectValue[Const] :
 *   - { }
 *   - { ObjectField[?Const]+ }
 */
and parseObject = (lexer: Lexer.t, ~isConst: bool) => {
  expect(lexer, BRACE_L)
  ->Result.flatMap(() => {
      let rec makeFields = (fields: result(list('a))) =>
        Result.flatMap(fields, fields =>
          if (!skip(lexer, BRACE_R)) {
            parseObjectField(lexer, ~isConst)
            ->Result.flatMap(field => makeFields(Ok([field, ...fields])));
          } else {
            Ok(fields);
          }
        );

      makeFields(Ok([]))->Result.map(fields => `Map(Belt.List.reverse(fields)));
    });
}

/**
 * ObjectField[Const] : Name : Value[?Const]
 */
and parseObjectField = (lexer: Lexer.t, ~isConst: bool): result((string, value)) => {
  parseName(lexer)
  ->Result.flatMap(name =>
      expect(lexer, COLON)
      ->Result.flatMap(() =>
          parseValueLiteral(lexer, ~isConst)->Result.map(value => (name, value))
        )
    );
};

let rec parseTypeReference = (lexer: Lexer.t) => {
  let typ =
    if (skip(lexer, BRACKET_L)) {
      parseTypeReference(lexer)
      ->Result.flatMap(t => expect(lexer, BRACKET_R)->Result.map(() => ListType(t)));
    } else {
      parseNamedType(lexer);
    };

  Result.map(typ, typ => skip(lexer, BANG) ? NonNullType(typ) : typ);
};

let parseArgument = (lexer: Lexer.t, ~isConst): result((string, value)) => {
  parseName(lexer)
  ->Result.flatMap(name =>
      expect(lexer, COLON)
      ->Result.flatMap(() =>
          parseValueLiteral(lexer, ~isConst)->Result.map(valueLiteral => (name, valueLiteral))
        )
    );
};

let parseArguments = (lexer: Lexer.t, ~isConst: bool) =>
  switch (lexer.token.kind) {
  | PAREN_L => many(lexer, PAREN_L, parseArgument(~isConst), PAREN_R)
  | _ => Ok([])
  };

let parseDirective = (lexer: Lexer.t, ~isConst: bool): result(directive) => {
  expect(lexer, AT)
  ->Result.flatMap(() => parseName(lexer))
  ->Result.flatMap(name =>
      parseArguments(lexer, ~isConst)->Result.map(args => {name, arguments: args})
    );
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
      typeCondition: hasTypeCondition ? Some(parseName(lexer)) : None,
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
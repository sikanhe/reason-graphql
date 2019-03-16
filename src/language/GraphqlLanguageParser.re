open GraphqlLanguageAst;
module Lexer = GraphqlLanguageLexer;

module Result = {
  include Belt.Result;
  let let_ = flatMap;
};

type result('a) = Result.t('a, GraphqlLanguageError.t);

let syntaxError = a => Result.Error(GraphqlLanguageError.SyntaxError(a));

/**
 * If the next token is of the given kind, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and throw an error.
 */
let expect = (lexer: Lexer.t, kind: Lexer.tokenKind) =>
  if (lexer.token.kind != kind) {
    syntaxError(
      "Expected" ++ Lexer.strOfTokenKind(kind) ++ ", found " ++ Lexer.printToken(lexer.token),
    );
  } else {
    Lexer.advance(lexer);
    Ok();
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
    ) => {
  let%Result () = expect(lexer, openKind);
  let rec collect = nodes =>
    if (!skip(lexer, closeKind)) {
      let%Result node = parseFn(lexer);
      collect([node, ...nodes]);
    } else {
      Ok(Belt.List.reverse(nodes));
    };

  collect([]);
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
      parseFn: Lexer.t => result('a),
      closeKind: Lexer.tokenKind,
    )
    : result(list('a)) => {
  let%Result () = expect(lexer, openKind);
  let%Result node = parseFn(lexer);

  let rec collect = nodes =>
    if (!skip(lexer, closeKind)) {
      let%Result node = parseFn(lexer);
      collect([node, ...nodes]);
    } else {
      Ok(Belt.List.reverse(nodes));
    };

  collect([node]);
};

let parseStringLiteral = ({token} as lexer: Lexer.t) => {
  Lexer.advance(lexer);
  `String(token.value);
};

let parseName = (lexer: Lexer.t) => {
  let token = lexer.token;
  let%Result () = expect(lexer, NAME);
  Ok(token.value);
};

let parseNamedType = (lexer: Lexer.t) => {
  let%Result name = parseName(lexer);
  Ok(NamedType(name));
};

/**
 * Variable : $ Name
 */
let parseVariable = (lexer: Lexer.t) => {
  let%Result () = expect(lexer, DOLLAR);
  let%Result name = parseName(lexer);
  Ok(`Variable(name));
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
    Ok(`Int(int_of_string(token.value)));
  | FLOAT =>
    Lexer.advance(lexer);
    Ok(`Float(float_of_string(token.value)));
  | STRING => Ok(parseStringLiteral(lexer))
  | NAME =>
    let value =
      switch (token.value) {
      | "true" => `Boolean(true)
      | "false" => `Boolean(false)
      | "null" => `Null
      | enum => `Enum(enum)
      };
    Lexer.advance(lexer);
    Ok(value);
  | DOLLAR when !isConst => parseVariable(lexer)
  | _ => unexpected(lexer)
  }

/**
 * ListValue[Const] :
 *   - [ ]
 *   - [ Value[?Const]+ ]
 */
and parseList = (lexer: Lexer.t, ~isConst: bool) => {
  let%Result list = any(lexer, BRACKET_L, parseValueLiteral(~isConst), BRACKET_R);
  Ok(`List(list));
}

/**
 * ObjectValue[Const] :
 *   - { }
 *   - { ObjectField[?Const]+ }
 */
and parseObject = (lexer: Lexer.t, ~isConst: bool) => {
  let%Result () = expect(lexer, BRACE_L);

  let rec parseFields = fields =>
    if (!skip(lexer, BRACE_R)) {
      let%Result field = parseObjectField(lexer, ~isConst);
      parseFields([field, ...fields]);
    } else {
      Ok(fields);
    };

  let%Result fields = parseFields([]);
  Ok(`Map(Belt.List.reverse(fields)));
}

/**
 * ObjectField[Const] : Name : Value[?Const]
 */
and parseObjectField = (lexer: Lexer.t, ~isConst: bool): result((string, value)) => {
  let%Result name = parseName(lexer);
  let%Result () = expect(lexer, COLON);
  let%Result value = parseValueLiteral(lexer, ~isConst);
  Ok((name, value));
};

let rec parseTypeReference = (lexer: Lexer.t) => {
  let%Result typ =
    if (skip(lexer, BRACKET_L)) {
      let%Result t = parseTypeReference(lexer);
      let%Result () = expect(lexer, BRACKET_R);
      Ok(ListType(t));
    } else {
      let%Result typ = parseNamedType(lexer);
      Ok(typ);
    };

  skip(lexer, BANG) ? Ok(NonNullType(typ)) : Ok(typ);
};

let parseArgument = (lexer: Lexer.t, ~isConst): result((string, value)) => {
  let%Result name = parseName(lexer);
  let%Result () = expect(lexer, COLON);
  let%Result valueLiteral = parseValueLiteral(lexer, ~isConst);
  Ok((name, valueLiteral));
};

let parseArguments = (lexer: Lexer.t, ~isConst: bool) =>
  switch (lexer.token.kind) {
  | PAREN_L => many(lexer, PAREN_L, parseArgument(~isConst), PAREN_R)
  | _ => Ok([])
  };

let parseDirective = (lexer: Lexer.t, ~isConst: bool): result(directive) => {
  let%Result () = expect(lexer, AT);
  let%Result name = parseName(lexer);
  let%Result arguments = parseArguments(lexer, ~isConst);
  Ok({name, arguments}: directive);
};

let parseDirectives = (lexer: Lexer.t, ~isConst: bool) => {
  let rec collect = directives =>
    if (lexer.token.kind == AT) {
      let%Result directive = parseDirective(lexer, ~isConst);
      collect([directive, ...directives]);
    } else {
      Ok(Belt.List.reverse(directives));
    };

  collect([]);
};

/* Operation Definitions */

let parseOperationType = (lexer: Lexer.t) => {
  let operationToken = lexer.token;
  let%Result () = expect(lexer, NAME);
  switch (operationToken.value) {
  | "query" => Ok(Query)
  | "mutation" => Ok(Mutation)
  | "subscription" => Ok(Subscription)
  | _ => unexpected(lexer, ~atToken=operationToken)
  };
};

let parseVariableDefinition = (lexer: Lexer.t): result(variableDefinition) => {
  let%Result variable = parseVariable(lexer);
  let%Result () = expect(lexer, COLON);
  let%Result typ = parseTypeReference(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=true);
  Ok({typ, variable, defaultValue: None, directives});
};

let parseVariableDefinitions = (lexer: Lexer.t) =>
  switch (lexer.token.kind) {
  | PAREN_L => many(lexer, PAREN_L, parseVariableDefinition, PAREN_R)
  | _ => Ok([])
  };

/**
 * SelectionSet : { Selection+ }
 */
let rec parseSelectionSet = (lexer: Lexer.t): result(list(selection)) =>
  many(lexer, BRACE_L, parseSelection, BRACE_R)

/**
 * Selection :
 *   - Field
 *   - FragmentSpread
 *   - InlineFragment
 */
and parseSelection = (lexer: Lexer.t): result(selection) =>
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
  let%Result () = expect(lexer, SPREAD);

  let hasTypeCondition = skipKeyword(lexer, "on");

  switch (lexer.token.kind) {
  | NAME when !hasTypeCondition =>
    let%Result name = parseFragmentName(lexer);
    let%Result directives = parseDirectives(lexer, ~isConst=false);
    Ok(FragmentSpread({name, directives}));
  | _ =>
    let typeCondition =
      hasTypeCondition ?
        switch (parseName(lexer)) {
        | Ok(name) => Some(name)
        | _ => None
        } :
        None;
    let%Result directives = parseDirectives(lexer, ~isConst=false);
    let%Result selectionSet = parseSelectionSet(lexer);
    Ok(InlineFragment({typeCondition, directives, selectionSet}));
  };
}

and parseField = (lexer: Lexer.t) => {
  let%Result name = parseName(lexer);

  let%Result (alias, name) =
    if (skip(lexer, COLON)) {
      let%Result name2 = parseName(lexer);
      Ok((Some(name), name2));
    } else {
      Ok((None, name));
    };

  let%Result arguments = parseArguments(lexer, ~isConst=false);
  let%Result directives = parseDirectives(lexer, ~isConst=false);
  let%Result selectionSet =
    switch (lexer.token.kind) {
    | BRACE_L => parseSelectionSet(lexer)
    | _ => Ok([])
    };

  Ok(Field({name, alias, arguments, directives, selectionSet}));
};

/**
 * OperationDefinition :
 *  - SelectionSet
 *  - OperationType Name? VariableDefinitions? Directives? SelectionSet
 */
let parseOperationDefinition = (lexer: Lexer.t) =>
  switch (lexer.token.kind) {
  | BRACE_L =>
    let%Result selectionSet = parseSelectionSet(lexer);
    Ok(
      OperationDefinition({
        operationType: Query,
        name: None,
        variableDefinition: [],
        directives: [],
        selectionSet,
      }),
    );
  | _ =>
    let%Result operationType = parseOperationType(lexer);
    let%Result name =
      switch (lexer.token.kind) {
      | NAME =>
        let%Result name = parseName(lexer);
        Ok(Some(name));
      | _ => Ok(None)
      };

    let%Result variableDefinition = parseVariableDefinitions(lexer);
    let%Result directives = parseDirectives(lexer, ~isConst=false);
    let%Result selectionSet = parseSelectionSet(lexer);

    Ok(OperationDefinition({operationType, name, variableDefinition, directives, selectionSet}));
  };

/**
 * FragmentDefinition :
 *   - fragment FragmentName on TypeCondition Directives? SelectionSet
 *
 * TypeCondition : NamedType
 */
let parseFragmentDefinition = (lexer: Lexer.t) => {
  let%Result () = expectKeyword(lexer, "fragment");
  let%Result name = parseFragmentName(lexer);
  let%Result () = expectKeyword(lexer, "on");

  let%Result typeCondition = parseName(lexer);
  let%Result selectionSet = parseSelectionSet(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=false);

  Ok(FragmentDefinition({typeCondition, name, selectionSet, directives}));
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
let parseDocument = (lexer: Lexer.t): result(document) => {
  let%Result definitions = many(lexer, SOF, parseDefinition, EOF);
  Ok({definitions: definitions});
};

let parse = (body: string) => {
  let lexer = Lexer.make(body);
  parseDocument(lexer)
};

let parseExn = (body: string) => parse(body)->Result.getExn
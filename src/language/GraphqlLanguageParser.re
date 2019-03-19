open GraphqlLanguageAst;
module Lexer = GraphqlLanguageLexer;

module Result = {
  include Belt.Result;
  let let_ = flatMap;
};

type result('a) = Result.t('a, GraphqlLanguageError.t);

let syntaxError = a => Result.Error(GraphqlLanguageError.SyntaxError(a));

let expectedError = (lexer: Lexer.t, token: Lexer.token) => {
  syntaxError(
    "Expected" ++ Lexer.tokenKind(token) ++ ", found " ++ Lexer.printToken(lexer.token),
  );
};

/**
 * If the next token is of the given kind, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and throw an error.
 */
let expect = (lexer: Lexer.t, token: Lexer.token) =>
  switch (fst(lexer.token)) {
  | currToken when currToken == token => Lexer.advance(lexer)
  | _ => expectedError(lexer, token)
  };

/**
 * If the next token is of the given kind, return true after advancing
 * the lexer. Otherwise, do not change the parser state and return false.
 */
let skip = (lexer: Lexer.t, skipToken: Lexer.token): result(bool) =>
  switch (fst(lexer.token)) {
  | token when token == skipToken =>
    let%Result _ = Lexer.advance(lexer);
    Ok(true);
  | _ => Ok(false)
  };

/**
 * If the next token is a keyword with the given value, return true after advancing
 * the lexer. Otherwise, do not change the parser state and return false.
 */
let skipKeyword = (lexer: Lexer.t, value: string): result(bool) =>
  switch (fst(lexer.token)) {
  | Name(name) when name == value =>
    let%Result _ = Lexer.advance(lexer);
    Ok(true);
  | _ => Ok(false)
  };

/**
 * If the next token is a keyword with the given value, return that token after
 * advancing the lexer. Otherwise, do not change the parser state and throw
 * an error.
 */
let expectKeyword = (lexer: Lexer.t, value: string): result(unit) => {
  let%Result skipped = skipKeyword(lexer, value);
  if (!skipped) {
    syntaxError("Expected " ++ value ++ ", found " ++ Lexer.printToken(lexer.token));
  } else {
    Ok();
  };
};

/**
 * Helper function for creating an error when an unexpected lexed token
 * is encountered.
 */
let unexpected = (lexer: Lexer.t) => {
  syntaxError("Unexpected " ++ Lexer.printToken(lexer.token));
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
      openKind: Lexer.token,
      parseFn: Lexer.t => result('a),
      closeKind: Lexer.token,
    ) => {
  let%Result _ = expect(lexer, openKind);

  let rec collect = nodes => {
    let%Result skipped = skip(lexer, closeKind);
    if (!skipped) {
      let%Result node = parseFn(lexer);
      collect([node, ...nodes]);
    } else {
      Ok(Belt.List.reverse(nodes));
    };
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
      openKind: Lexer.token,
      parseFn: Lexer.t => result('a),
      closeKind: Lexer.token,
    )
    : result(list('a)) => {
  let%Result _ = expect(lexer, openKind);
  let%Result node = parseFn(lexer);

  let rec collect = nodes => {
    let%Result skipped = skip(lexer, closeKind);
    if (!skipped) {
      let%Result node = parseFn(lexer);
      collect([node, ...nodes]);
    } else {
      Ok(Belt.List.reverse(nodes));
    };
  };

  collect([node]);
};

let parseName =
  fun
  | ({token: (Name(value), _)} as lexer: Lexer.t) => {
      let%Result _ = Lexer.advance(lexer);
      Result.Ok(value);
    }
  | lexer => expectedError(lexer, Name(""));

let parseNamedType = (lexer: Lexer.t) => {
  let%Result name = parseName(lexer);
  Ok(NamedType(name));
};

/**
 * Variable : $ Name
 */
let parseVariable = (lexer: Lexer.t) => {
  let%Result _ = expect(lexer, Dollar);
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
let rec parseValueLiteral =
        ({token: (token, _)} as lexer: Lexer.t, ~isConst: bool): result(value) =>
  switch (token) {
  | BracketOpen =>
    let%Result list = any(lexer, BracketOpen, parseValueLiteral(~isConst), BracketClose);
    Ok(`List(list));
  | BraceOpen => parseObject(lexer, ~isConst)
  | Int(value) =>
    let%Result _ = Lexer.advance(lexer);
    Ok(`Int(int_of_string(value)));
  | Float(value) =>
    let%Result _ = Lexer.advance(lexer);
    Ok(`Float(float_of_string(value)));
  | String(value) =>
    let%Result _ = Lexer.advance(lexer);
    Ok(`String(value));
  | Name(value) =>
    let value =
      switch (value) {
      | "true" => `Boolean(true)
      | "false" => `Boolean(false)
      | "null" => `Null
      | enum => `Enum(enum)
      };
    let%Result _ = Lexer.advance(lexer);
    Ok(value);
  | Dollar when !isConst => parseVariable(lexer)
  | _ => unexpected(lexer)
  }

/**
 * ObjectValue[Const] :
 *   - { }
 *   - { ObjectField[?Const]+ }
 */
and parseObject = (lexer: Lexer.t, ~isConst: bool) => {
  let%Result _ = expect(lexer, BraceOpen);

  let rec parseFields = fields => {
    let%Result skipped = skip(lexer, BraceClose);
    if (!skipped) {
      let%Result field = parseObjectField(lexer, ~isConst);
      parseFields([field, ...fields]);
    } else {
      Ok(fields);
    };
  };

  let%Result fields = parseFields([]);
  Ok(`Map(Belt.List.reverse(fields)));
}

/**
 * ObjectField[Const] : Name : Value[?Const]
 */
and parseObjectField = (lexer: Lexer.t, ~isConst: bool): result((string, value)) => {
  let%Result name = parseName(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result value = parseValueLiteral(lexer, ~isConst);
  Ok((name, value));
};

let rec parseTypeReference = (lexer: Lexer.t) => {
  let%Result typ = {
    let%Result skipped = skip(lexer, BracketOpen);
    if (skipped) {
      let%Result t = parseTypeReference(lexer);
      let%Result _ = expect(lexer, BracketClose);
      Ok(ListType(t));
    } else {
      let%Result typ = parseNamedType(lexer);
      Ok(typ);
    };
  };

  let%Result skipped = skip(lexer, Bang);

  skipped ? Ok(NonNullType(typ)) : Ok(typ);
};

let parseArgument = (lexer: Lexer.t, ~isConst): result((string, value)) => {
  let%Result name = parseName(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result valueLiteral = parseValueLiteral(lexer, ~isConst);
  Ok((name, valueLiteral));
};

let parseArguments = (lexer: Lexer.t, ~isConst: bool) =>
  switch (fst(lexer.token)) {
  | ParenOpen => many(lexer, ParenOpen, parseArgument(~isConst), ParenClose)
  | _ => Ok([])
  };

let parseDirective = (lexer: Lexer.t, ~isConst: bool): result(directive) => {
  let%Result _ = expect(lexer, At);
  let%Result name = parseName(lexer);
  let%Result arguments = parseArguments(lexer, ~isConst);
  Ok({name, arguments}: directive);
};

let parseDirectives = (lexer: Lexer.t, ~isConst: bool) => {
  let rec collect = directives =>
    switch (fst(lexer.token)) {
    | At =>
      let%Result directive = parseDirective(lexer, ~isConst);
      collect([directive, ...directives]);
    | _ => Ok(Belt.List.reverse(directives))
    };

  collect([]);
};

/* Operation Definitions */

let parseOperationType = (lexer: Lexer.t): result(GraphqlLanguageAst.operationType) => {
  switch (fst(lexer.token)) {
  | Name("query") => Result.Ok(Query)
  | Name("mutation") => Ok(Mutation)
  | Name("subscription") => Ok(Subscription)
  | _ => unexpected(lexer)
  };
};

let parseVariableDefinition = (lexer: Lexer.t): result(variableDefinition) => {
  let%Result variable = parseVariable(lexer);
  let%Result _ = expect(lexer, Colon);
  let%Result typ = parseTypeReference(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=true);
  Ok({typ, variable, defaultValue: None, directives});
};

let parseVariableDefinitions = (lexer: Lexer.t) =>
  switch (fst(lexer.token)) {
  | ParenOpen => many(lexer, ParenOpen, parseVariableDefinition, ParenClose)
  | _ => Ok([])
  };

/**
 * SelectionSet : { Selection+ }
 */
let rec parseSelectionSet = (lexer: Lexer.t): result(list(selection)) =>
  many(lexer, BraceOpen, parseSelection, BraceClose)

/**
 * Selection :
 *   - Field
 *   - FragmentSpread
 *   - InlineFragment
 */
and parseSelection = (lexer: Lexer.t): result(selection) =>
  switch (fst(lexer.token)) {
  | Spread => parseFragment(lexer)
  | _ => parseField(lexer)
  }

/**
 * FragmentName : Name but not `on`
 */
and parseFragmentName = (lexer: Lexer.t) =>
  switch (fst(lexer.token)) {
  | Name("on") => unexpected(lexer)
  | _ => parseName(lexer)
  }

/**
 * Corresponds to both FragmentSpread and InlineFragment in the spec.
 * FragmentSpread : ... FragmentName Directives?
 * InlineFragment : ... TypeCondition? Directives? SelectionSet
 */
and parseFragment = (lexer: Lexer.t) => {
  let%Result _ = expect(lexer, Spread);
  let%Result hasTypeCondition = skipKeyword(lexer, "on");

  switch (fst(lexer.token)) {
  | Name(_) when !hasTypeCondition =>
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

  let%Result (alias, name) = {
    let%Result skipped = skip(lexer, Colon);
    if (skipped) {
      let%Result name2 = parseName(lexer);
      Ok((Some(name), name2));
    } else {
      Ok((None, name));
    };
  };

  let%Result arguments = parseArguments(lexer, ~isConst=false);
  let%Result directives = parseDirectives(lexer, ~isConst=false);
  let%Result selectionSet =
    switch (fst(lexer.token)) {
    | BraceOpen => parseSelectionSet(lexer)
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
  switch (fst(lexer.token)) {
  | BraceOpen =>
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
    let%Result _ = Lexer.advance(lexer);
    let%Result name =
      switch (lexer.token) {
      | (Name(name), _) =>
        let%Result _ = Lexer.advance(lexer);
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
let parseExecutableDefinition = (lexer: Lexer.t) =>
  switch (fst(lexer.token)) {
  | Name("query" | "mutation" | "subscription")
  | BraceOpen => parseOperationDefinition(lexer)
  | Name("fragment") => parseFragmentDefinition(lexer)
  | _ => unexpected(lexer)
  };

/**
 * Document : Definition+
 */
let parseDocument = (lexer: Lexer.t): result(document) => {
  let%Result definitions = many(lexer, StartOfFile, parseExecutableDefinition, EndOfFile);
  Ok({definitions: definitions});
};

let parse = (body: string) => {
  let lexer = Lexer.make(body);
  parseDocument(lexer);
};
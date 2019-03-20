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
    "Expected" ++ Lexer.tokenKind(token) ++ ", found " ++ Lexer.tokenDesc(lexer.token),
  );
};

let expect = (lexer: Lexer.t, token: Lexer.token) =>
  switch (fst(lexer.token)) {
  | currToken when currToken == token => Lexer.advance(lexer)
  | _ => expectedError(lexer, token)
  };

let skip = (lexer: Lexer.t, skipToken: Lexer.token): result(bool) =>
  switch (fst(lexer.token)) {
  | token when token == skipToken => Lexer.advance(lexer)->Result.map(_ => true)
  | _ => Ok(false)
  };

let skipKeyword = (lexer: Lexer.t, value: string): result(bool) =>
  switch (fst(lexer.token)) {
  | Name(name) when name == value => Lexer.advance(lexer)->Result.map(_ => true)
  | _ => Ok(false)
  };

let expectKeyword = (lexer: Lexer.t, value: string): result(unit) => {
  let%Result skipped = skipKeyword(lexer, value);
  if (!skipped) {
    syntaxError("Expected " ++ value ++ ", found " ++ Lexer.tokenDesc(lexer.token));
  } else {
    Ok();
  };
};

let unexpected = (lexer: Lexer.t) => {
  syntaxError("Unexpected " ++ Lexer.tokenDesc(lexer.token));
};

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
      Lexer.advance(lexer)->Result.map(_ => value);
    }
  | lexer => expectedError(lexer, Name(""));

let parseNamedType = (lexer: Lexer.t) => {
  let%Result name = parseName(lexer);
  Ok(NamedType(name));
};

let parseVariable = (lexer: Lexer.t) => {
  let%Result _ = expect(lexer, Dollar);
  let%Result name = parseName(lexer);
  Ok(`Variable(name));
};

let rec parseValueLiteral = (lexer: Lexer.t, ~isConst: bool): result(value) =>
  Result.(
    switch (fst(lexer.token)) {
    | BracketOpen =>
      any(lexer, BracketOpen, parseValueLiteral(~isConst), BracketClose)
      ->map(list => `List(list))
    | BraceOpen => parseObject(lexer, ~isConst)
    | Int(value) => Lexer.advance(lexer)->map(_ => `Int(int_of_string(value)))
    | Float(value) => Lexer.advance(lexer)->map(_ => `Float(float_of_string(value)))
    | String(value) => Lexer.advance(lexer)->map(_ => `String(value))
    | Name("true") => Lexer.advance(lexer)->map(_ => `Boolean(true))
    | Name("false") => Lexer.advance(lexer)->map(_ => `Boolean(false))
    | Name("null") => Lexer.advance(lexer)->map(_ => `Null)
    | Name(value) => Lexer.advance(lexer)->map(_ => `Enum(value))
    | Dollar when !isConst => parseVariable(lexer)
    | _ => unexpected(lexer)
    }
  )

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

let rec parseSelectionSet = (lexer: Lexer.t): result(list(selection)) => {
  many(lexer, BraceOpen, parseSelection, BraceClose);
}

and parseSelection = (lexer: Lexer.t): result(selection) =>
  switch (fst(lexer.token)) {
  | Spread => parseFragment(lexer)
  | _ => parseField(lexer)
  }

and parseFragmentName = (lexer: Lexer.t) =>
  switch (fst(lexer.token)) {
  | Name("on") => unexpected(lexer)
  | _ => parseName(lexer)
  }

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
      | (Name(name), _) => Lexer.advance(lexer)->Result.map(_ => Some(name))
      | _ => Ok(None)
      };

    let%Result variableDefinition = parseVariableDefinitions(lexer);
    let%Result directives = parseDirectives(lexer, ~isConst=false);
    let%Result selectionSet = parseSelectionSet(lexer);

    Ok(OperationDefinition({operationType, name, variableDefinition, directives, selectionSet}));
  };

let parseFragmentDefinition = (lexer: Lexer.t) => {
  let%Result () = expectKeyword(lexer, "fragment");
  let%Result name = parseFragmentName(lexer);
  let%Result () = expectKeyword(lexer, "on");

  let%Result typeCondition = parseName(lexer);
  let%Result selectionSet = parseSelectionSet(lexer);
  let%Result directives = parseDirectives(lexer, ~isConst=false);

  Ok(FragmentDefinition({typeCondition, name, selectionSet, directives}));
};

let parseExecutableDefinition = (lexer: Lexer.t) =>
  switch (fst(lexer.token)) {
  | Name("query" | "mutation" | "subscription")
  | BraceOpen => parseOperationDefinition(lexer)
  | Name("fragment") => parseFragmentDefinition(lexer)
  | _ => unexpected(lexer)
  };

let parseDocument = (lexer: Lexer.t): result(document) => {
  let%Result definitions = many(lexer, StartOfFile, parseExecutableDefinition, EndOfFile);
  Ok({definitions: definitions});
};

let parse = (body: string) => {
  let lexer = Lexer.make(body);
  parseDocument(lexer);
};
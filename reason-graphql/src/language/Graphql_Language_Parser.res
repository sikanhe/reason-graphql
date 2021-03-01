open Graphql_Language_Ast
module Lexer = Graphql_Language_Lexer

let syntaxError = Lexer.syntaxError

let expectedError = (lexer: Lexer.t, token: Lexer.token) =>
  syntaxError("Expected" ++ (Lexer.tokenKind(token) ++ (", found " ++ Lexer.tokenDesc(lexer.curr))))

let expect = (lexer: Lexer.t, token: Lexer.token) =>
  switch lexer.curr.token {
  | currToken if currToken == token => Lexer.advance(lexer)
  | _ => expectedError(lexer, token)
  }

let skip = (lexer: Lexer.t, skipToken: Lexer.token): bool =>
  switch lexer.curr.token {
  | token if token == skipToken => {
      let _ = Lexer.advance(lexer)
      true
    }
  | _ => false
  }

let skipKeyword = (lexer: Lexer.t, value: string): bool =>
  switch lexer.curr.token {
  | Name(name) if name == value =>
    let _ = Lexer.advance(lexer)
    true
  | _ => false
  }

let expectKeyword = (lexer: Lexer.t, value: string) => {
  let skipped = skipKeyword(lexer, value)
  if !skipped {
    syntaxError("Expected " ++ (value ++ (", found " ++ Lexer.tokenDesc(lexer.curr))))
  } else {
    ()
  }
}

let unexpected = (lexer: Lexer.t) => syntaxError("Unexpected " ++ Lexer.tokenDesc(lexer.curr))

let any = (
  lexer: Lexer.t,
  openKind: Lexer.token,
  parseFn: Lexer.t => 'a,
  closeKind: Lexer.token,
) => {
  let _ = expect(lexer, openKind)

  let rec collect = nodes => {
    let skipped = skip(lexer, closeKind)
    if !skipped {
      let node = parseFn(lexer)
      collect(list{node, ...nodes})
    } else {
      Belt.List.reverse(nodes)
    }
  }

  collect(list{})
}

let many = (
  lexer: Lexer.t,
  openKind: Lexer.token,
  parseFn: Lexer.t => 'a,
  closeKind: Lexer.token,
): list<'a> => {
  let _ = expect(lexer, openKind)

  let node = parseFn(lexer)

  let rec collect = nodes => {
    let skipped = skip(lexer, closeKind)
    if !skipped {
      let node = parseFn(lexer)
      collect(list{node, ...nodes})
    } else {
      Belt.List.reverse(nodes)
    }
  }

  collect(list{node})
}

let parseName = x =>
  switch x {
  | ({curr: {token: Name(name)}} as lexer: Lexer.t) =>
    let _ = Lexer.advance(lexer)
    name
  | lexer => expectedError(lexer, Name(""))
  }

let parseNamedType = (lexer: Lexer.t) => {
  let name = parseName(lexer)
  NamedType(name)
}

let parseVariable = (lexer: Lexer.t) => {
  let _ = expect(lexer, Dollar)

  let name = parseName(lexer)
  #Variable(name)
}

let rec parseValueLiteral = (lexer: Lexer.t, ~isConst: bool): value => {
  switch lexer.curr.token {
  | BracketOpen =>
    let list = any(lexer, BracketOpen, parseValueLiteral(~isConst), BracketClose)
    #List(list)
  | BraceOpen => parseObject(lexer, ~isConst)
  | Int(value) =>
    let _ = Lexer.advance(lexer)
    #Int(int_of_string(value))
  | Float(value) =>
    let _ = Lexer.advance(lexer)
    #Float(float_of_string(value))
  | String(value) =>
    let _ = Lexer.advance(lexer)
    #String(value)
  | Name("true") =>
    let _ = Lexer.advance(lexer)
    #Boolean(true)
  | Name("false") =>
    let _ = Lexer.advance(lexer)
    #Boolean(false)
  | Name("null") =>
    let _ = Lexer.advance(lexer)
    #Null
  | Name(value) =>
    let _ = Lexer.advance(lexer)
    #Enum(value)
  | Dollar if !isConst => parseVariable(lexer)
  | _ => unexpected(lexer)
  }
}

and parseObject = (lexer: Lexer.t, ~isConst: bool) => {
  let _ = expect(lexer, BraceOpen)

  let rec parseFields = fields => {
    let skipped = skip(lexer, BraceClose)
    if !skipped {
      let field = parseObjectField(lexer, ~isConst)
      parseFields(list{field, ...fields})
    } else {
      fields
    }
  }

  let fields = parseFields(list{})
  #Object(Belt.List.reverse(fields))
}

and parseObjectField = (lexer: Lexer.t, ~isConst: bool): (string, value) => {
  let name = parseName(lexer)
  let _ = expect(lexer, Colon)
  let value = parseValueLiteral(lexer, ~isConst)
  (name, value)
}

let rec parseTypeReference = (lexer: Lexer.t) => {
  let typ = {
    let skipped = skip(lexer, BracketOpen)
    if skipped {
      let t = parseTypeReference(lexer)

      let _ = expect(lexer, BracketClose)
      ListType(t)
    } else {
      let typ = parseNamedType(lexer)
      typ
    }
  }

  let skipped = skip(lexer, Bang)
  skipped ? NonNullType(typ) : typ
}

let parseArgument = (lexer: Lexer.t, ~isConst): (string, value) => {
  let name = parseName(lexer)

  let _ = expect(lexer, Colon)
  let valueLiteral = parseValueLiteral(lexer, ~isConst)
  (name, valueLiteral)
}

let parseArguments = (lexer: Lexer.t, ~isConst: bool) =>
  switch lexer.curr.token {
  | ParenOpen => many(lexer, ParenOpen, parseArgument(~isConst), ParenClose)
  | _ => list{}
  }

let parseDirective = (lexer: Lexer.t, ~isConst: bool): directive => {
  let _ = expect(lexer, At)
  let name = parseName(lexer)
  let arguments = parseArguments(lexer, ~isConst)
  ({name: name, arguments: arguments}: directive)
}

let parseDirectives = (lexer: Lexer.t, ~isConst: bool) => {
  let rec collect = directives =>
    switch lexer.curr.token {
    | At =>
      let directive = parseDirective(lexer, ~isConst)
      collect(list{directive, ...directives})

    | _ => Belt.List.reverse(directives)
    }

  collect(list{})
}

/* Operation Definitions */

let parseOperationType = (lexer: Lexer.t): Graphql_Language_Ast.operationType =>
  switch lexer.curr.token {
  | Name("query") => Query
  | Name("mutation") => Mutation
  | Name("subscription") => Subscription
  | _ => unexpected(lexer)
  }

let parseVariableDefinition = (lexer: Lexer.t): variableDefinition => {
  let variable = parseVariable(lexer)
  let _ = expect(lexer, Colon)
  let typ = parseTypeReference(lexer)
  let directives = parseDirectives(lexer, ~isConst=true)
  {typ: typ, variable: variable, defaultValue: None, directives: directives}
}

let parseVariableDefinitions = (lexer: Lexer.t) =>
  switch lexer.curr.token {
  | ParenOpen => many(lexer, ParenOpen, parseVariableDefinition, ParenClose)
  | _ => list{}
  }

let rec parseSelectionSet = (lexer: Lexer.t): list<selection> =>
  many(lexer, BraceOpen, parseSelection, BraceClose)

and parseSelection = (lexer: Lexer.t): selection =>
  switch lexer.curr.token {
  | Spread => parseFragment(lexer)
  | _ => parseField(lexer)
  }

and parseFragmentName = (lexer: Lexer.t) =>
  switch lexer.curr.token {
  | Name("on") => unexpected(lexer)
  | _ => parseName(lexer)
  }

and parseFragment = (lexer: Lexer.t) => {
  let _ = expect(lexer, Spread)

  let hasTypeCondition = skipKeyword(lexer, "on")

  switch lexer.curr.token {
  | Name(_) if !hasTypeCondition =>
    let name = parseFragmentName(lexer)
    let directives = parseDirectives(lexer, ~isConst=false)
    FragmentSpread({name: name, directives: directives})

  | _ =>
    let typeCondition = if hasTypeCondition {
      Some(parseName(lexer))
    } else {
      None
    }

    let directives = parseDirectives(lexer, ~isConst=false)
    let selectionSet = parseSelectionSet(lexer)

    InlineFragment({
      typeCondition: typeCondition,
      directives: directives,
      selectionSet: selectionSet,
    })
  }
}

and parseField = (lexer: Lexer.t) => {
  let name = parseName(lexer)

  let (alias, name) = {
    let skipped = skip(lexer, Colon)
    if skipped {
      let name2 = parseName(lexer)
      (Some(name), name2)
    } else {
      (None, name)
    }
  }

  let arguments = parseArguments(lexer, ~isConst=false)
  let directives = parseDirectives(lexer, ~isConst=false)
  let selectionSet = switch lexer.curr.token {
  | BraceOpen => parseSelectionSet(lexer)
  | _ => list{}
  }

  Field({
    name: name,
    alias: alias,
    arguments: arguments,
    directives: directives,
    selectionSet: selectionSet,
  })
}

let parseOperationDefinition = (lexer: Lexer.t) =>
  switch lexer.curr.token {
  | BraceOpen =>
    let selectionSet = parseSelectionSet(lexer)
    Operation({
      operationType: Query,
      name: None,
      variableDefinition: list{},
      directives: list{},
      selectionSet: selectionSet,
    })
  | _ =>
    let operationType = parseOperationType(lexer)
    let _ = Lexer.advance(lexer)

    let name = switch lexer.curr.token {
    | Name(name) =>
      let _ = Lexer.advance(lexer)
      Some(name)
    | _ => None
    }

    let variableDefinition = parseVariableDefinitions(lexer)
    let directives = parseDirectives(lexer, ~isConst=false)
    let selectionSet = parseSelectionSet(lexer)

    Operation({
      operationType: operationType,
      name: name,
      variableDefinition: variableDefinition,
      directives: directives,
      selectionSet: selectionSet,
    })
  }

let parseFragmentDefinition = (lexer: Lexer.t) => {
  expectKeyword(lexer, "fragment")
  let name = parseFragmentName(lexer)
  expectKeyword(lexer, "on")
  let typeCondition = parseName(lexer)
  let selectionSet = parseSelectionSet(lexer)
  let directives = parseDirectives(lexer, ~isConst=false)

  Fragment({
    typeCondition: typeCondition,
    name: name,
    selectionSet: selectionSet,
    directives: directives,
  })
}

let parseExecutableDefinition = (lexer: Lexer.t) =>
  switch lexer.curr.token {
  | Name("query" | "mutation" | "subscription")
  | BraceOpen =>
    parseOperationDefinition(lexer)
  | Name("fragment") => parseFragmentDefinition(lexer)
  | _ => unexpected(lexer)
  }

let parseDocument = (lexer: Lexer.t): document => {
  let definitions = many(lexer, StartOfFile, parseExecutableDefinition, EndOfFile)
  {definitions: definitions}
}

let parse = (body: string) => {
  let lexer = Lexer.make(body)
  try {
    Ok(parseDocument(lexer))
  } catch {
  | Lexer.SyntaxError(_) as err => Error(err)
  }
}

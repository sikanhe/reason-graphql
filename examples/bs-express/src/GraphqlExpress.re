open Express;

type parseBodyResult =
  | DocumentAndVariables(
      Graphql.Language.Ast.document,
      list((string, Graphql.Language.Ast.constValue)),
    )
  | Document(Graphql.Language.Ast.document)
  | ParseError(string);

let parseBodyIntoDocumentAndVariables = req => {
  switch (Request.bodyJSON(req)) {
  | Some(body) =>
    switch (Js.Json.decodeObject(body)) {
    | Some(body) =>
      switch (Js.Dict.get(body, "query")) {
      | Some(query) =>
        switch (Js.Json.decodeString(query)) {
        | Some(queryString) =>
          switch (Graphql.Language.Parser.parse(queryString)) {
          | Ok(document) =>
            switch (Js.Dict.get(body, "variables")) {
            | Some(variablesJson) =>
              switch (Graphql.Json.toVariables(variablesJson)) {
              | Ok(variables) => DocumentAndVariables(document, variables)
              | Error(e) => ParseError(e)
              }
            | None => Document(document)
            }
          | Error(SyntaxError(s)) =>
            ParseError("GraphQL Syntax Error: " ++ s)
          }
        | None => ParseError("Query must be a string")
        }
      | None => ParseError("Must provide Query string")
      }
    | None => ParseError("body must be an JSON object")
    }
  | None => ParseError("no body found")
  };
};

let middleware = (~provideCtx, ~graphiql=false, schema) =>
  PromiseMiddleware.from((next, req, res) =>
    switch (Request.methodRaw(req)) {
    | "GET" when graphiql =>
      res
      |> Response.status(Response.StatusCode.Ok)
      |> Response.sendString("TODO: Show Graphiql UI")
      |> Js.Promise.resolve
    | "POST" =>
      switch (parseBodyIntoDocumentAndVariables(req)) {
      | DocumentAndVariables(document, variables) =>
        GraphqlFuture.Schema.execute(
          schema,
          ~document,
          ~variables,
          ~ctx=provideCtx(req),
        )
        ->Future.map(Graphql.Json.fromConstValue)
        ->Future.map(json => Response.sendJson(json, res))
        ->FutureJs.toPromise
      | Document(document) =>
        GraphqlFuture.Schema.execute(schema, ~document, ~ctx=provideCtx(req))
        ->Future.map(Graphql.Json.fromConstValue)
        ->Future.map(json => Response.sendJson(json, res))
        ->FutureJs.toPromise
      | ParseError(error) =>
        res
        |> Response.status(Response.StatusCode.BadRequest)
        |> Response.sendString(error)
        |> Js.Promise.resolve
      }
    | _ => Js.Promise.resolve(next(Next.route, res))
    }
  );
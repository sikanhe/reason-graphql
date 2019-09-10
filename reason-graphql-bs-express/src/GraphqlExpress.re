open Express;

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
              | Ok(variables) => Belt.Result.Ok((document, Some(variables)))
              | Error(_) => Ok((document, None))
              }
            | None => Ok((document, None))
            }
          | Error(SyntaxError(s)) => Error("GraphQL Syntax Error: " ++ s)
          }
        | None => Error("Query must be a string")
        }
      | None => Error("Must provide Query string")
      }
    | None => Error("body must be an JSON object")
    }
  | None => Error("no body found")
  };
};

let middleware = (~provideCtx, ~graphiql=false, schema) =>
  PromiseMiddleware.from((next, req, res) =>
    switch (Request.methodRaw(req)) {
    | "GET" when graphiql =>
      res
      |> Response.status(Response.StatusCode.Ok)
      |> Response.sendString(Graphiql.html)
      |> Js.Promise.resolve
    | "POST" =>
      switch (parseBodyIntoDocumentAndVariables(req)) {
      | Ok((document, variables)) =>
        GraphqlPromise.Schema.execute(schema, ~document, ~variables?, ~ctx=provideCtx(req))
        |> Js.Promise.(then_(const => resolve(Graphql.Json.fromConstValue(const))))
        |> Js.Promise.(then_(json => resolve(Response.sendJson(json, res))))
      | Error(error) =>
        res
        |> Response.status(Response.StatusCode.BadRequest)
        |> Response.sendString(error)
        |> Js.Promise.resolve
      }
    | _ => Js.Promise.resolve(next(Next.route, res))
    }
  );
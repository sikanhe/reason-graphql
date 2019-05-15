open Express;

let app = express();

App.use(app, Middleware.json(~limit=ByteLimit.mb(5.0), ()));

App.useOnPath(app, ~path="/graphql") @@
GraphqlExpress.middleware(Schema.schema, ~provideCtx=_ => (), ~graphiql=true);

App.get(app, ~path="*") @@
Middleware.from((_, _) => Response.redirect("/graphql"));

let onListen = e =>
  switch (e) {
  | exception (Js.Exn.Error(e)) =>
    Js.log(e);
    Node.Process.exit(1);
  | _ => Js.log("Listening at http://127.0.0.1:3000")
  };

App.listen(app, ~port=3000, ~onListen, ());
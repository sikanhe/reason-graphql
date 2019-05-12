type ctx = {userIP: string};

module HelloWorldSchema = {
  open GraphqlFuture;

  let rootQuery =
    Schema.(
      query([
        field(
          "hello",
          nonnull(string),
          ~args=Arg.[defaultArg("name", string, ~default="world")],
          ~resolve=(ctx, (), name) =>
          name ++ " (" ++ ctx.userIP ++ ")"
        ),
      ])
    );

  let schema = Schema.create(rootQuery);
};

open Express;

let app = express();

App.use(app, Middleware.json(~limit=ByteLimit.mb(5.0), ()));

App.useOnPath(app, ~path="/graphql") @@
GraphqlExpress.middleware(
  HelloWorldSchema.schema,
  ~provideCtx=req => {userIP: Request.ip(req)},
  ~graphiql=true,
);

let onListen = e =>
  switch (e) {
  | exception (Js.Exn.Error(e)) =>
    Js.log(e);
    Node.Process.exit(1);
  | _ => Js.log("Listening at http://127.0.0.1:3000")
  };

App.listen(app, ~port=3000, ~onListen, ());
module Schema =
  Graphql.Schema.Make({
    type t('a) = Js.Promise.t('a);

    let return = Js.Promise.resolve;
    let map = (p, continuation) => {
      p |> Js.Promise.then_(value => continuation(value)->Js.Promise.resolve);
    };
    let bind = (p, continuation) => {
      Js.Promise.then_(continuation, p);
    };
  });

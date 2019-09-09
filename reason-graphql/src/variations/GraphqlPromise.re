module Promise = {
  type t('a) = Js.Promise.t('a);

  let return = Js.Promise.resolve;

  let bind = (p, continuation) => {
    Js.Promise.then_(continuation, p);
  };

  let ok = a => return(Belt.Result.Ok(a));
};

module Schema = Graphql.Schema.Make(Promise);
module Language = Graphql.Language;

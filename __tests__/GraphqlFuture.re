module Future = {
  include Future;
  let return = value;
  let bind = flatMap;
  let ok = x => value(Belt.Result.Ok(x));
};

module Schema = GraphqlSchema.Make(Future);
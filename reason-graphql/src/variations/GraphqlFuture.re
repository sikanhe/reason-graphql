module Future = {
  include Future;
  let return = value;
  let bind = flatMap;
  let ok = x => value(Belt.Result.Ok(x));

  module Stream = {
    type t('a);
    let iter = Obj.magic();
    let map = Obj.magic();
    let close = Obj.magic();
  }
};

module Schema = Graphql.Schema.Make(Future);
module Language = Graphql.Language;

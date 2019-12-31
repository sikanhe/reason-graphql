module Schema =
  Graphql.Schema.Make({
    include Future;
    let return = value;
    let bind = flatMap;
  });

module Language = Graphql.Language;
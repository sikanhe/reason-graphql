module Schema =
  Graphql.Schema.Make({
    type t('a) = Promise.t('a);

    let return = Promise.resolved;
    let bind = Promise.flatMap;
  });

module Language = Graphql.Language;
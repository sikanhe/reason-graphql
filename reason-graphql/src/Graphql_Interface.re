module type IO = {
  type t(+'a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let map: (t('a), 'a => 'b) => t('b);
};

module type Schema = {
  module Io: IO;

  type variableList = list((string, Graphql_Language.Ast.constValue));

  type deprecation =
    | NotDeprecated
    | Deprecated(option(string));
  type enumValue('a) = {
    name: string,
    description: option(string),
    deprecated: deprecation,
    value: 'a,
  };
  type enum('a) = {
    name: string,
    description: option(string),
    values: list(enumValue('a)),
  };
  module Arg: {
    type arg(_) =
      | Arg(argument('a)): arg('a)
      | DefaultArg(argumentWithDefault('a)): arg('a)
    and argTyp(_) =
      | Scalar(scalar('a)): argTyp(option('a))
      | Enum(enum('a)): argTyp(option('a))
      | InputObject(inputObject('a, 'b)): argTyp(option('a))
      | List(argTyp('a)): argTyp(option(array('a)))
      | NonNull(argTyp(option('a))): argTyp('a)
    and scalar('a) = {
      name: string,
      description: option(string),
      parse: Graphql_Language.Ast.constValue => result('a, string),
    }
    and inputObject('a, 'b) = {
      name: string,
      description: option(string),
      fields: arglist('a, 'b),
      coerce: 'b,
    }
    and argument('a) = {
      name: string,
      description: option(string),
      typ: argTyp('a),
    }
    and argumentWithDefault('a) = {
      name: string,
      description: option(string),
      typ: argTyp(option('a)),
      default: 'a,
    }
    and arglist(_, _) =
      | []: arglist('a, 'a)
      | ::(arg('a), arglist('b, 'c)): arglist('b, 'a => 'c);
    let arg: (~description: string=?, string, argTyp('a)) => arg('a);
    let defaultArg:
      (~description: string=?, ~default: 'a, string, argTyp(option('a))) =>
      arg('a);
    let string: argTyp(option(string));
    let int: argTyp(option(int));
    let float: argTyp(option(float));
    let boolean: argTyp(option(bool));
    let list: argTyp('a) => argTyp(option(array('a)));
    let nonnull: argTyp(option('a)) => argTyp('a);
  };
  type typ(_, _) =
    | Scalar(scalar('src)): typ('ctx, option('src))
    | Enum(enum('src)): typ('ctx, option('src))
    | List(typ('ctx, 'src)): typ('ctx, option(array('src)))
    | Object(obj('ctx, 'src)): typ('ctx, option('src))
    | Abstract(abstract): typ('ctx, option(abstractValue('ctx, 'a)))
    | NonNull(typ('ctx, option('src))): typ('ctx, 'src)
  and scalar('src) = {
    name: string,
    description: option(string),
    serialize: 'src => Graphql_Language.Ast.constValue,
  }
  and obj('ctx, 'src) = {
    name: string,
    description: option(string),
    fields: Lazy.t(list(field('ctx, 'src))),
    abstracts: ref(list(abstract)),
  }
  and field(_, _) =
    | Field(fieldDefinition('src, 'out, 'ctx, 'a, 'args)): field('ctx, 'src)
  and fieldDefinition('src, 'out, 'ctx, 'a, 'args) = {
    name: string,
    description: option(string),
    deprecated: deprecation,
    typ: typ('ctx, 'out),
    args: Arg.arglist('a, 'args),
    resolve: ('ctx, 'src) => 'args,
    lift: 'a => Io.t(result('out, string)),
  }
  and anyTyp =
    | AnyTyp(typ('a, 'b)): anyTyp
    | AnyArgTyp(Arg.argTyp('c)): anyTyp
  and abstract = {
    name: string,
    description: option(string),
    mutable types: list(anyTyp),
    kind: [ | `Interface(Lazy.t(list(abstractField))) | `Union],
  }
  and abstractField =
    | AbstractField(field('a, 'b)): abstractField
  and abstractValue('ctx, 'a) =
    | AbstractValue((typ('ctx, option('src)), 'src))
      : abstractValue('ctx, 'a);
  type abstractType('ctx, 'a) = typ('ctx, option(abstractValue('ctx, 'a)));
  type directiveLocation = [
    | `Field
    | `FragmentDefinition
    | `FragmentSpread
    | `InlineFragment
    | `Mutation
    | `Query
    | `Subscription
    | `VariableDefinition
  ];
  type directiveInfo('args) = {
    name: string,
    description: option(string),
    locations: list(directiveLocation),
    args: Arg.arglist([ | `Include | `Skip], 'args),
    resolve: 'args,
  };
  type directive =
    | Directive(directiveInfo('args)): directive;
  let skipDirective: directive;
  let includeDirective: directive;
  type schema('ctx) = {
    query: obj('ctx, unit),
    mutation: option(obj('ctx, unit)),
  };
  type combinedEnum('ctx, 'a) = {
    argTyp: Arg.argTyp('a),
    fieldType: typ('ctx, 'a),
  };
  let makeEnum:
    (string, ~description: string=?, list(enumValue('a))) =>
    combinedEnum('b, option('a));
  let enumValue:
    (~description: string=?, ~deprecated: deprecation=?, ~value: 'a, string) =>
    enumValue('a);
  let obj:
    (
      ~description: string=?,
      ~implements: ref(list(abstract))=?,
      ~fields: typ('a, option('b)) => list(field('a, 'b)),
      string
    ) =>
    typ('a, option('b));
  let field:
    (
      ~description: string=?,
      ~deprecated: deprecation=?,
      ~args: Arg.arglist('a, 'b),
      ~resolve: ('c, 'd) => 'b,
      string,
      typ('c, 'a)
    ) =>
    field('c, 'd);
  let async_field:
    (
      ~description: string=?,
      ~deprecated: deprecation=?,
      ~args: Arg.arglist(Io.t(result('a, string)), 'b),
      ~resolve: ('c, 'd) => 'b,
      string,
      typ('c, 'a)
    ) =>
    field('c, 'd);
  let abstractField:
    (
      ~description: string=?,
      ~deprecated: deprecation=?,
      ~args: Arg.arglist('a, 'b),
      string,
      typ('c, 'a)
    ) =>
    abstractField;
  let union:
    (~description: string=?, string) =>
    typ('a, option(abstractValue('a, 'b)));
  let interface:
    (
      ~description: string=?,
      ~fields: typ('a, option(abstractValue('a, 'b))) =>
               list(abstractField),
      string
    ) =>
    typ('a, option(abstractValue('a, 'b)));
  let addType:
    (typ('a, option(abstractValue('a, 'b))), typ('c, option('d)), 'd) =>
    abstractValue('c, 'e);
  let query: list(field('ctx, unit)) => obj('ctx, unit);
  let mutation: list(field('ctx, unit)) => obj('ctx, unit);
  let create: (~mutation: obj('a, unit)=?, obj('a, unit)) => schema('a);
  let string: typ('ctx, option(string));
  let int: typ('ctx, option(int));
  let float: typ('ctx, option(float));
  let boolean: typ('ctx, option(bool));
  let list: typ('a, 'b) => typ('a, option(array('b)));
  let nonnull: typ('a, option('b)) => typ('a, 'b);
  type path = list(string);
  type error = (string, path);
  type resolveError = [
    | `ArgumentError(string)
    | `ResolveError(error)
    | `ValidationError(string)
  ];
  type executeError = [
    | `ArgumentError(string)
    | `MutationsNotConfigured
    | `NoOperationFound
    | `OperationNameRequired
    | `OperationNotFound
    | `ResolveError(error)
    | `SubscriptionsNotConfigured
    | `ValidationError(string)
  ];
  type executionResult = {data: Graphql_Language.Ast.constValue};
  module ArgEval: {
    let stringOfConstValue: Graphql_Language.Ast.constValue => string;
    let stringOfArgType: Arg.argTyp('a) => string;
    let evalArgError:
      (
        ~fieldType: string=?,
        ~fieldName: string,
        ~argName: string,
        Arg.argTyp('a),
        option(Graphql_Language.Ast.constValue)
      ) =>
      string;
  };
  let matchesTypeCondition: (string, obj('ctx, 'src)) => bool;
  let fieldName: Graphql_Language.Ast.field => string;
  let getObjField: (string, obj('ctx, 'src)) => option(field('ctx, 'src));
  let coerceOrNull:
    (option('a), 'a => Io.t(result([> | `Null] as 'b, 'c))) =>
    Io.t(result('b, 'c));
  let collectOperations:
    Graphql_Language.Ast.document =>
    list(Graphql_Language.Ast.operationDefinition);
  exception FragmentCycle(list(string));
  let okResponse: 'a => [> | `Object(list((string, 'a)))];
  let errorResponse:
    (~path: list(string)=?, string) => Graphql_Language.Ast.constValue;
  let execute:
    (
      ~variables: variableList=?,
      ~document: Graphql_Language.Ast.document,
      schema('ctx),
      ~ctx: 'ctx
    ) =>
    Io.t(Graphql_Language.Ast.constValue);
  let resultToJson: Io.t(Graphql_Language.Ast.constValue) => Io.t(Js.Json.t);
};

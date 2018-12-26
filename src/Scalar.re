module type ScalarDef = {
  let name: string;
  let description: option(string);
  let parse: Ast.constValue => Belt.Result.t('src, string);
  let serialize: 'src => Ast.constValue;
};

module Make = (ScalarDef: ScalarDef) => {
  let argType =
    Schema.Arg.{name: ScalarDef.name, parse: ScalarDef.parse, description: ScalarDef.description};

  let fieldType =
    Schema.{
      name: ScalarDef.name,
      serialize: ScalarDef.serialize,
      description: ScalarDef.description,
    };
};
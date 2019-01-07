module type ScalarDef = {
  let name: string;
  let description: option(string);
  let parse: Language.Ast.constValue => Belt.Result.t('src, string);
  let serialize: 'src => Language.Ast.constValue;
};

module Make = (ScalarDef: ScalarDef) => {
  let arg =
    Schema.Arg.{name: ScalarDef.name, parse: ScalarDef.parse, description: ScalarDef.description};

  let field =
    Schema.{
      name: ScalarDef.name,
      serialize: ScalarDef.serialize,
      description: ScalarDef.description,
    };
};
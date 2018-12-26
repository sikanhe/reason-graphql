module Option = {
  let map = (x, ~f) =>
    switch (x) {
    | None => None
    | Some(y) => Some(f(y))
    };
};

module StringMap = {
  include Map.Make(String);
  exception Missing_key(string);
  let find_exn = (key, t) =>
    try (find(key, t)) {
    | Not_found => raise(Missing_key(key))
    };
  let find = (k, t) =>
    try (Some(find_exn(k, t))) {
    | Missing_key(_) => None
    };
};

type deprecation =
  | NotDeprecated
  | Deprecated
  | DeprecatedWithReason(string);

let rec serializeValue: Ast.constValue => Js.Json.t =
  fun
  | `String(string)
  | `Enum(string) => Js.Json.string(string)
  | `Float(float) => Js.Json.number(float)
  | `Int(int) => Js.Json.number(float_of_int(int))
  | `Boolean(bool) => Js.Json.boolean(bool)
  | `List(list) =>
    list |> List.map(item => serializeValue(item)) |> Array.of_list |> Js.Json.array
  | `Map(assocList) => {
      let dict = Js.Dict.empty();
      assocList |> List.iter(((name, value)) => Js.Dict.set(dict, name, serializeValue(value)));
      Js.Json.object_(dict);
    }
  | `Null => Js.Json.null;

type enum('a) = {
  name: string,
  description: option(string),
  values: list(enumValue('a)),
}

and enumValue('a) = {
  name: string,
  description: option(string),
  deprecated: deprecation,
  value: 'a,
};

module Arg = {
  type arg(_) =
    | Arg(argument('a)): arg('a)
    | ArgWithDefault(argumentWithDefault('a)): arg('a)
  and argument('a) = {
    name: string,
    description: option(string),
    typ: argType('a),
  }
  and argumentWithDefault('a) = {
    name: string,
    description: option(string),
    typ: argType('a),
    default: 'a,
  }
  and argType(_) =
    | Scalar(scalar('a)): argType('a)
    | InputObject(inputObject('a, 'b)): argType('a)
    | Nullable(argType('a)): argType('a)
    | List(argType('a)): argType(list('a))
  and scalar('a) = {
    name: string,
    description: option(string),
    parse: Ast.constValue => Belt.Result.t('a, string),
  }
  and inputObject('a, 'b) = {
    name: string,
    description: option(string),
    fields: arglist('a, 'b),
    coerce: 'b,
  }
  and arglist(_, _) =
    | []: arglist('a, 'a)
    | ::(arg('a), arglist('b, 'c)): arglist('b, 'a => 'c);

  let arg = (~description=?, name, typ) => Arg({name, typ, description});

  let arg' = (~description=?, ~default, name, typ) =>
    ArgWithDefault({name, typ, description, default});

  type variables = StringMap.t(Ast.constValue);

  let rec value_to_const_value: (variables, Ast.value) => Ast.constValue =
    variable_map =>
      fun
      | `Null => `Null
      | `Int(_) as i => i
      | `Float(_) as f => f
      | `String(_) as s => s
      | `Boolean(_) as b => b
      | `Enum(_) as e => e
      | `Variable(v) => StringMap.find_exn(v, variable_map)
      | `List(xs) => `List(List.map(value_to_const_value(variable_map), xs))
      | `Map(props) => {
          let props' =
            List.map(
              ((name, value)) => (name, value_to_const_value(variable_map, value)),
              props,
            );
          `Map(props');
        };

  let (>>=) = (result: Belt.Result.t('a, 'b), f) => {
    switch (result) {
    | Ok(v) => f(v)
    | Error(_) as e => e
    };
  };

  let (>>|) = (x, f) => Belt.Result.map(x, f);

  let rec string_of_const_value: Ast.constValue => string = (
    fun
    | `Null => "null"
    | `Int(i) => string_of_int(i)
    | `Float(f) => string_of_float(f)
    | `String(s) => Printf.sprintf("\"%s\"", s)
    | `Boolean(b) => string_of_bool(b)
    | `Enum(e) => e
    | `List(l) => {
        let values = List.map(i => string_of_const_value(i), l);
        Printf.sprintf("[%s]", String.concat(", ", values));
      }
    | `Map(a) => {
        let values =
          List.map(((k, v)) => Printf.sprintf("%s: %s", k, string_of_const_value(v)), a);

        Printf.sprintf("{%s}", String.concat(", ", values));
      }:
      Ast.constValue => string
  );

  let rec string_of_arg_typ: type a. argType(a) => string =
    fun
    | Scalar(a) => Printf.sprintf("%s!", a.name)
    | InputObject(a) => Printf.sprintf("%s!", a.name)
    /* | Enum(a) => a.name */
    | List(a) => Printf.sprintf("[%s]", string_of_arg_typ(a))
    | Nullable(a) => Printf.sprintf("%s", string_of_arg_typ(a));

  let eval_arg_error = (~field_type="field", ~field_name, ~arg_name, arg_typ, value) => {
    let found_str =
      switch (value) {
      | Some(v) => Printf.sprintf("found %s", string_of_const_value(v))
      | None => "but not provided"
      };

    Printf.sprintf(
      "Argument `%s` of type `%s` expected on %s `%s`, %s.",
      arg_name,
      string_of_arg_typ(arg_typ),
      field_type,
      field_name,
      found_str,
    );
  };


  let rec eval_arglist:
    type a b.
      (
        variables,
        ~field_type: string=?,
        ~field_name: string,
        arglist(a, b),
        list((string, Ast.value)),
        b
      ) =>
      Belt.Result.t(a, string) =
    (variable_map, ~field_type=?, ~field_name, arglist, key_values, f) =>
      switch (arglist) {
      | [] => Ok(f)
      /* | [ArgWithDefault(arg), ...arglist'] =>
         let arglist'' = [
           Arg({name: arg.name, description: arg.description, typ: arg.typ}),
           ...arglist',
         ];
         eval_arglist(
           variable_map,
           ~field_type?,
           ~field_name,
           arglist'',
           key_values,
           fun 
           | Some(v) => f(v)
           | None => arg.default
         ); */
      | [Arg(arg), ...arglist'] =>
        try (
          {
            let value = Belt.List.getAssoc(key_values, arg.name, (==));
            let const_value = Belt.Option.map(value, value_to_const_value(variable_map));
            eval_arg(
              variable_map,
              ~field_type?,
              ~field_name,
              ~arg_name=arg.name,
              arg.typ,
              const_value,
            )
            >>= (
              coerced =>
                eval_arglist(
                  variable_map,
                  ~field_type?,
                  ~field_name,
                  arglist',
                  key_values,
                  f(coerced),
                )
            );
          }
        ) {
        | StringMap.Missing_key(key) => Error(Format.sprintf("Missing variable `%s`", key))
        }
      }

  and eval_arg:
    type a.
      (
        variables,
        ~field_type: string=?,
        ~field_name: string,
        ~arg_name: string,
        argType(a),
        option(Ast.constValue)
      ) =>
      Belt.Result.t(a, string) =
    (variable_map, ~field_type=?, ~field_name, ~arg_name, typ, value) =>
      switch (typ, value) {
      /* | (Nullable(_), Some(`Null)) => Ok(None)
         | (Nullable(_), None) => Ok(None)
         | (_, None) => Error(eval_arg_error(~field_type?, ~field_name, ~arg_name, typ, value))
         | (_, Some(`Null)) =>
           Error(eval_arg_error(~field_type?, ~field_name, ~arg_name, typ, value)) */
      | (Scalar(s), Some(value)) =>
        switch (s.parse(value)) {
        | Ok(coerced) => Ok(coerced)
        | Error(_) =>
          Error(eval_arg_error(~field_type?, ~field_name, ~arg_name, typ, Some(value)))
        }
      | (InputObject(o), Some(value)) =>
        switch (value) {
        | `Map(props) =>
          let props' = (props :> list((string, Ast.value)));
          eval_arglist(variable_map, ~field_type?, ~field_name, o.fields, props', o.coerce);

        | _ => Error(eval_arg_error(~field_type?, ~field_name, ~arg_name, typ, Some(value)))
        }
      /* | (List(typ), Some(value)) =>
        switch (value) {
        | `List(values) =>
          values |> List.map(eval_arg(variable_map, ~field_type?, ~field_name, ~arg_name, typ))
          let option_values = List.map(x => Some(x), values);
          Belt.Result.(
            option_values,
            eval_arg(variable_map, ~field_type?, ~field_name, ~arg_name, typ),
          )
          >>| (coerced => Some(coerced));
        | value =>
          eval_arg(variable_map, ~field_type?, ~field_name, ~arg_name, typ, Some(value))
          >>| ((coerced) => (Some([coerced]): a))
        } */
      | (Nullable(typ), value) =>
        eval_arg(variable_map, ~field_type?, ~field_name, ~arg_name, typ, value)
      /* | (Enum(e), Some(value)) =>
         switch (value) {
         | `Enum(v)
         | `String(v) =>
           switch (List.find(enum_value => enum_value.name == v, e.values)) {
           | Some(enum_value) => Ok(Some(enum_value.value))
           | None =>
             Error(
               Printf.sprintf(
                 "Invalid enum value for argument `%s` on field `%s`",
                 arg_name,
                 field_name,
               ),
             )
           }
         | _ =>
           Error(
             Printf.sprintf("Expected enum for argument `%s` on field `%s`", arg_name, field_name),
           )
         } */
      };

  /* Built in scalars */

  let string =
    Scalar({
      name: "String",
      description: None,

      parse: input =>
        switch (input) {
        | `String(str) => Ok(str)
        | _ => failwith("Not a string")
        },
    });

  let int =
    Scalar({
      name: "Int",
      description: None,
      parse: input =>
        switch (input) {
        | `Int(int) => Ok(int)
        | _ => failwith("Not an integer")
        },
    });

  let float =
    Scalar({
      name: "Float",
      description: None,
      parse: input =>
        switch (input) {
        | `Float(float) => Ok(float)
        | _ => failwith("Not a float")
        },
    });

  let boolean =
    Scalar({
      name: "Boolean",
      description: None,
      parse: input =>
        switch (input) {
        | `Boolean(bool) => Ok(bool)
        | _ => failwith("Not a boolean")
        },
    });
};

type typ(_) =
  | Scalar(scalar('src)): typ('src)
  | Enum(enum('src)): typ('src)
  | List(typ('src)): typ(list('src))
  | Object(obj('src)): typ('src)
  | Interface(interface('src)): typ('src)
  | Nullable(typ('src)): typ(option('src))
and scalar('src) = {
  name: string,
  description: option(string),
  serialize: 'src => Ast.constValue,
}
and obj('src) = {
  name: string,
  description: option(string),
  fields: Lazy.t(list(field('src))),
  implements: list(interface('src)),
}
and interface('src) = {
  name: string,
  description: option(string),
  fields: Lazy.t(list(field('src))),
}
and field('src) =
  | Field(fieldDefinition('src, 'out, 'a, 'args)): field('src)
and fieldDefinition('src, 'out, 'a, 'args) = {
  name: string,
  description: option(string),
  deprecated: deprecation,
  typ: typ('out),
  arguments: Arg.arglist('a, 'args),
  resolve: 'src => 'args,
  lift: 'a => 'out,
};

type t = {
  query: obj(unit),
  /* mutation: obj(unit), */
};

let create = (~query) => {query: query};

let field = (~description=?, ~args, ~deprecated=NotDeprecated, name, typ, resolve) =>
  Field({name, typ, resolve, deprecated, description, arguments: args, lift: a => a});

let enum = (~description=?, ~values, name) => Enum({name, description, values});

let enumValue = (~description=?, ~deprecated=NotDeprecated, ~value, name) => {
  name,
  description,
  deprecated,
  value,
};

let obj = (~description=?, ~implements: list(interface('src))=[], ~fields, name) => {
  let rec self = Object({name, description, fields: lazy (fields(self)), implements});
  self;
};

let rootQuery = (fields): obj(unit) => {
  name: "RootQueryType",
  description: None,
  fields: lazy fields,
  implements: [],
};

/* Built in scalars */
let string = Scalar({name: "String", description: None, serialize: str => `String(str)});
let int = Scalar({name: "Int", description: None, serialize: int => `Int(int)});
let float = Scalar({name: "Float", description: None, serialize: float => `Float(float)});
let boolean = Scalar({name: "Boolean", description: None, serialize: bool => `Boolean(bool)});
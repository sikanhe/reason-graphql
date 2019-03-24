let rec fromConstValue: GraphqlLanguageAst.constValue => Js.Json.t =
  fun
  | `String(string)
  | `Enum(string) => Js.Json.string(string)
  | `Float(float) => Js.Json.number(float)
  | `Int(int) => Js.Json.number(float_of_int(int))
  | `Boolean(bool) => Js.Json.boolean(bool)
  | `List(list) =>
    Belt.List.map(list, item => fromConstValue(item)) |> Belt.List.toArray |> Js.Json.array
  | `Map(assocList) => {
      let dict =
        Belt.List.reduceReverse(
          assocList,
          Js.Dict.empty(),
          (dict, (name, value)) => {
            Js.Dict.set(dict, name, fromConstValue(value));
            dict;
          },
        );
      Js.Json.object_(dict);
    }
  | `Null => Js.Json.null;

let rec toConstValue = (json: Js.Json.t): GraphqlLanguageAst.constValue =>
  switch (Js.Json.classify(json)) {
  | JSONString(value) => `String(value)
  | JSONNumber(num) when Js.Math.floor_float(num) == num => `Int(int_of_float(num))
  | JSONNumber(num) => `Float(num)
  | JSONTrue => `Boolean(true)
  | JSONFalse => `Boolean(false)
  | JSONNull => `Null
  | JSONArray(array) =>
    `List(Belt.Array.map(array, item => toConstValue(item)) |> Belt.List.fromArray)
  | JSONObject(dict) =>
    `Map(
      Js.Dict.entries(dict)
      ->Belt.Array.map(((k, v)) => (k, toConstValue(v)))
      ->Belt.List.fromArray,
    )
  };

let toVariables =
    (json: Js.Json.t): Belt.Result.t(list((string, GraphqlLanguageAst.constValue)), string) => {
  switch (Js.Json.classify(json)) {
  | JSONObject(dict) =>
    Ok(
      Js.Dict.entries(dict)
      ->Belt.Array.map(((k, v)) => (k, toConstValue(v)))
      ->Belt.List.fromArray
    )
  | _ => Error("Variables must be a JSON object")
  };
};
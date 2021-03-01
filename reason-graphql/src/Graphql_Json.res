let rec fromConstValue: Graphql_Language_Ast.constValue => Js.Json.t = x =>
  switch x {
  | #String(string)
  | #Enum(string) =>
    Js.Json.string(string)
  | #Float(float) => Js.Json.number(float)
  | #Int(int) => Js.Json.number(float_of_int(int))
  | #Boolean(bool) => Js.Json.boolean(bool)
  | #List(list) =>
    Belt.List.map(list, item => fromConstValue(item)) |> Belt.List.toArray |> Js.Json.array
  | #Object(rows) =>
    let dict = Belt.List.reduceReverse(rows, Js.Dict.empty(), (dict, (name, value)) => {
      Js.Dict.set(dict, name, fromConstValue(value))
      dict
    })
    Js.Json.object_(dict)
  | #Null => Js.Json.null
  }

let rec toConstValue = (json: Js.Json.t): Graphql_Language_Ast.constValue =>
  switch Js.Json.classify(json) {
  | JSONString(value) => #String(value)
  | JSONNumber(num) if Js.Math.floor_float(num) == num => #Int(int_of_float(num))
  | JSONNumber(num) => #Float(num)
  | JSONTrue => #Boolean(true)
  | JSONFalse => #Boolean(false)
  | JSONNull => #Null
  | JSONArray(array) =>
    #List(Belt.Array.map(array, item => toConstValue(item)) |> Belt.List.fromArray)
  | JSONObject(dict) =>
    #Object(
      Js.Dict.entries(dict)->Belt.Array.map(((k, v)) => (k, toConstValue(v)))->Belt.List.fromArray,
    )
  }

let toVariables = (json: Js.Json.t): Belt.Result.t<
  list<(string, Graphql_Language_Ast.constValue)>,
  string,
> =>
  switch Js.Json.classify(json) {
  | JSONObject(dict) =>
    Ok(Js.Dict.entries(dict)->Belt.Array.map(((k, v)) => (k, toConstValue(v)))->Belt.List.fromArray)
  | _ => Error("Variables must be a JSON object")
  }

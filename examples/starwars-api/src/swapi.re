type film = {
  title: string,
  episodeID: int,
  openingCrawl: string,
  director: string,
  producers: list(string),
  releaseDate: string,
};

type person = {
  name: string,
  birthYear: string,
  eyeColor: string,
  gender: string,
  hairColor: string,
  height: int,
  mass: float,
  skinColor: string,
};

type planet = {
  name: string,
  diameter: int,
  rotationPeriod: int,
  orbitalPeriod: int,
  gravity: string,
  population: float,
  climates: list(string),
  terrains: list(string),
  surfaceWater: float,
};

type species = {
  name: string,
  classification: string,
  designation: string,
  averageHeight: float,
  averageLifespan: int,
  eyeColors: list(string),
  hairColors: list(string),
  skinColors: list(string),
  language: string,
};

type starship = {
  name: string,
  model: string,
  starshipClass: string,
  manufacturers: list(string),
  costInCredits: option(float),
  length: float,
  crew: string,
  passengers: list(string),
  maxAtmospheringSpeed: option(int),
  hyperdriveRating: float,
  mglt: int,
  cargoCapacity: float,
  consumables: string,
};

type vehicle = {
  name: string,
  model: string,
  vehicleClass: string,
  manufacturers: list(string),
  costInCredits: option(float),
  length: float,
  crew: string,
  passengers: list(string),
  maxAtmospheringSpeed: option(int),
  cargoCapacity: float,
  consumables: string,
};

[%raw "require('isomorphic-fetch')"];

let baseUrl = "https://swapi.co/api/";

exception RecordNotFound;

let parseFloat =
  fun
  | "n/a"
  | "unknown" => None
  | n => Some(float_of_string(n));

let parseInt =
  fun
  | "n/a"
  | "unknown" => None
  | n => Some(int_of_string(n));

let getEntity = (path, decoder, id) =>
  Js.Promise.(
    Fetch.fetch(baseUrl ++ path ++ "/" ++ string_of_int(id))
    |> then_(res =>
         Fetch.Response.status(res) == 404
           ? reject(RecordNotFound) : resolve(res)
       )
    |> then_(Fetch.Response.json)
    |> then_(json => Some(decoder(json)) |> resolve)
  )
  ->FutureJs.fromPromise(Js.String.make);

let decodeFilm = (json: Js.Json.t): film =>
  Json.Decode.{
    title: json |> field("title", string),
    episodeID: json |> field("episode_id", int),
    openingCrawl: json |> field("opening_crawl", string),
    director: json |> field("director", string),
    producers:
      json
      |> field("producer", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    releaseDate: json |> field("release_date", string),
  };

let getFilm = getEntity("films", decodeFilm);

let decodePerson = (json: Js.Json.t): person =>
  Json.Decode.{
    name: json |> field("name", string),
    birthYear: json |> field("birth_year", string),
    eyeColor: json |> field("eye_color", string),
    gender: json |> field("gender", string),
    hairColor: json |> field("hair_color", string),
    height: json |> field("height", string) |> int_of_string,
    mass: json |> field("mass", string) |> float_of_string,
    skinColor: json |> field("skin_color", string),
  };

let getPerson = getEntity("people", decodePerson);

let decodePlanet = (json: Js.Json.t): planet =>
  Json.Decode.{
    name: json |> field("name", string),
    diameter: json |> field("diameter", string) |> int_of_string,
    rotationPeriod: json |> field("rotation_period", string) |> int_of_string,
    orbitalPeriod: json |> field("orbital_period", string) |> int_of_string,
    gravity: json |> field("gravity", string),
    population: json |> field("population", string) |> float_of_string,
    climates:
      json
      |> field("climate", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    terrains:
      json
      |> field("terrain", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    surfaceWater: json |> field("surface_water", string) |> float_of_string,
  };

let getPlanet = getEntity("planets", decodePlanet);

let decodeSpecies = (json: Js.Json.t): species =>
  Json.Decode.{
    name: json |> field("name", string),
    classification: json |> field("classification", string),
    designation: json |> field("designation", string),
    averageHeight: json |> field("average_height", string) |> float_of_string,
    averageLifespan:
      json |> field("average_lifespan", string) |> int_of_string,
    eyeColors:
      json
      |> field("eye_colors", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    hairColors:
      json
      |> field("hair_colors", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    skinColors:
      json
      |> field("skin_colors", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    language: json |> field("language", string),
  };

let getSpecies = getEntity("species", decodeSpecies);

let decodeStarship = (json: Js.Json.t): starship =>
  Json.Decode.{
    name: json |> field("name", string),
    model: json |> field("model", string),
    starshipClass: json |> field("starship_class", string),
    manufacturers:
      json
      |> field("manufacturer", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    costInCredits: json |> field("cost_in_credits", string) |> parseFloat,
    length: json |> field("length", string) |> float_of_string,
    crew: json |> field("crew", string),
    passengers:
      json
      |> field("passengers", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    maxAtmospheringSpeed:
      json |> field("max_atmosphering_speed", string) |> parseInt,
    hyperdriveRating:
      json |> field("hyperdrive_rating", string) |> float_of_string,
    mglt: json |> field("MGLT", string) |> int_of_string,
    cargoCapacity: json |> field("cargo_capacity", string) |> float_of_string,
    consumables: json |> field("consumables", string),
  };

let getStarship = getEntity("starships", decodeStarship);

let decodeVehicle = (json: Js.Json.t): vehicle =>
  Json.Decode.{
    name: json |> field("name", string),
    model: json |> field("model", string),
    vehicleClass: json |> field("vehicle_class", string),
    manufacturers:
      json
      |> field("manufacturer", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    costInCredits: json |> field("cost_in_credits", string) |> parseFloat,
    length: json |> field("length", string) |> float_of_string,
    crew: json |> field("crew", string),
    passengers:
      json
      |> field("passengers", string)
      |> Js.String.split(", ")
      |> Array.to_list,
    maxAtmospheringSpeed:
      json |> field("max_atmosphering_speed", string) |> parseInt,
    cargoCapacity: json |> field("cargo_capacity", string) |> float_of_string,
    consumables: json |> field("consumables", string),
  };

let getVehicle = getEntity("vehicles", decodeVehicle);
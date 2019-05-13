[%raw "require('isomorphic-fetch')"];

let baseUrl = "https://swapi.co/api/";

let decodeFilm = (json: Js.Json.t): Types.film =>
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

exception RecordNotFound;

let getFilm = id =>
  Js.Promise.
    (
      Fetch.fetch(baseUrl ++ "films/" ++ string_of_int(id))
      |> then_(res =>
           Fetch.Response.status(res) == 404
             ? reject(RecordNotFound) : resolve(res)
         )
      |> then_(Fetch.Response.json)
      |> then_(json => Some(decodeFilm(json)) |> resolve)
    )
  ->FutureJs.fromPromise(Js.String.make);
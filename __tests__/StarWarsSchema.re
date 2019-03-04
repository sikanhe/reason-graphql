open Belt.Result;
open GraphqlFuture;

module StarWars = StarWarsData;

let episodeEnum =
  Schema.(
    makeEnum(
      "Episode",
      [
        enumValue("NEWHOPE", ~value=StarWars.NEWHOPE, ~description="Released in 1977"),
        enumValue("EMPIRE", ~value=StarWars.EMPIRE, ~description="Released in 1980"),
        enumValue("JEDI", ~value=StarWars.JEDI, ~description="Released in 1983"),
      ],
    )
  );

let rec characterInterface: Schema.abstractType('ctx, [ | `Character]) =
  Schema.(
    interface("Character", ~fields=character =>
      [
        abstractField("id", nonNull(int), ~args=[]),
        abstractField("name", nonNull(string), ~args=[]),
        abstractField("appearsIn", nonNull(list(nonNull(episodeEnum.fieldType))), ~args=[]),
        abstractField("friends", nonNull(list(nonNull(character))), ~args=[]),
      ]
    )
  )

and humanAsCharacterInterface =
  lazy (Schema.addType(characterInterface, Lazy.force(humanTypeLazy)))
and droidAsCharacterInterface =
  lazy (Schema.addType(characterInterface, Lazy.force(droidTypeLazy)))
and asCharacterInterface =
  fun
  | StarWars.Human(human) => Lazy.force(humanAsCharacterInterface, human)
  | Droid(droid) => Lazy.force(droidAsCharacterInterface, droid)

and humanTypeLazy =
  lazy
    Schema.(
      obj("Human", ~description="A humanoid creature in the Star Wars universe.", ~fields=_ =>
        [
          field("id", nonNull(int), ~args=[], ~resolve=(_ctx, human: StarWars.human) => human.id),
          field("name", nonNull(string), ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
            human.StarWars.name
          ),
          field(
            "appearsIn",
            nonNull(list(nonNull(episodeEnum.fieldType))),
            ~args=[],
            ~resolve=(_ctx, human: StarWars.human) =>
            human.StarWars.appearsIn
          ),
          async_field(
            "friends",
            nonNull(list(nonNull(characterInterface))),
            ~args=[],
            ~resolve=(_ctx, human: StarWars.human) =>
            StarWars.getFriends(human.friends)
            ->Future.map(list => Belt.List.map(list, asCharacterInterface))
            ->Future.map(list => Ok(list))
          ),
          field("homePlanet", string, ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
            human.homePlanet
          ),
        ]
      )
    )

and droidTypeLazy =
  lazy
    Schema.(
      obj("Droid", ~description="A mechanical creature in the Star Wars universe.", ~fields=_ =>
        [
          field("id", nonNull(int), ~args=[], ~resolve=(_ctx, droid: StarWars.droid) => droid.id),
          field("name", nonNull(string), ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.StarWars.name
          ),
          field(
            "appearsIn",
            nonNull(list(nonNull(episodeEnum.fieldType))),
            ~args=[],
            ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.StarWars.appearsIn
          ),
          field(
            "primaryFunction", nonNull(string), ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.primaryFunction
          ),
          async_field(
            "friends",
            nonNull(list(nonNull(characterInterface))),
            ~args=[],
            ~resolve=(_ctx, droid: StarWars.droid) =>
            StarWars.getFriends(droid.friends)
            ->Future.map(list => Belt.List.map(list, asCharacterInterface))
            ->Future.map(list => Ok(list))
          ),
        ]
      )
    );

let humanType = Lazy.force(humanTypeLazy);
let droidType = Lazy.force(droidTypeLazy);
let humanAsCharacterInterface = Lazy.force(humanAsCharacterInterface);
let droidAsCharacterInterface = Lazy.force(droidAsCharacterInterface);

let query =
  Schema.(
    query([
      field(
        "hero",
        nonNull(characterInterface),
        ~args=
          Arg.[
            arg(
              "episode",
              episodeEnum.argTyp,
              ~description=
                "If omitted, returns the hero of the whole saga. "
                ++ "If provided, returns the hero of that particular episode.",
            ),
          ],
        ~resolve=(_ctx, (), episode) =>
        switch (episode) {
        | Some(EMPIRE) => humanAsCharacterInterface(StarWarsData.luke)
        | _ => droidAsCharacterInterface(StarWarsData.artoo)
        }
      ),
      async_field(
        "human",
        humanType,
        ~args=Arg.[arg("id", nonNull(int))],
        ~resolve=(_ctx, (), argId) => {
          let id = argId;
          StarWarsData.getHuman(id)->Future.map(human => Ok(human));
        },
      ),
      async_field(
        "droid",
        droidType,
        ~args=Arg.[arg("id", nonNull(int))],
        ~resolve=(_ctx, (), argId) => {
          let id = argId;
          StarWarsData.getDroid(id)->Future.map(droid => Ok(droid));
        },
      ),
    ])
  );

let updateCharacterResponse =
  Schema.(
    obj("UpdateCharacterResponse", ~fields=_ =>
      [
        field(
          "error",
          string,
          ~args=[],
          ~resolve=(_, updateCharResult: StarWars.updateCharacterResult) =>
          switch (updateCharResult) {
          | Ok(_) => None
          | Error(CharacterNotFound(id)) =>
            Some("Character with ID " ++ string_of_int(id) ++ " not found")
          }
        ),
        field(
          "character",
          characterInterface,
          ~args=[],
          ~resolve=(_, updateCharResult: StarWars.updateCharacterResult) =>
          switch (updateCharResult) {
          | Ok(char) => Some(char->asCharacterInterface)
          | Error(_) => None
          }
        ),
      ]
    )
  );

let mutation =
  Schema.(
    mutation([
      async_field(
        "updateCharacterName",
        nonNull(updateCharacterResponse),
        ~args=Arg.[arg("characterId", nonNull(int)), arg("name", nonNull(string))],
        ~resolve=(_ctx, (), charId, name) =>
        StarWarsData.updateCharacterName(charId, name)->Future.flatMap(Future.ok)
      ),
    ])
  );

let schema: Schema.schema(unit) = Schema.create(query, ~mutation);

[@bs.deriving {jsConverter: newType}]
type method = [
  | `GET
  | `POST
  | `PUT
  | `UPDATE
  | `DELETE
  | `HEAD
  | `OPTION
  | `CONNECT
  | `TRACE
  | `PATCH
];

let methodStr =
  fun
  | `GET => "GET"
  | `POST => "POST"
  | `PUT => "PUT"
  | `UPDATE => "UPDATE"
  | `DELETE => "DELETE"
  | `HEAD => "HEAD"
  | `OPTION => "OPTION"
  | `CONNECT => "CONNECT"
  | `TRACE => "TRACE"
  | `PATCH => "PATCH";

[@bs.deriving {jsConverter: newType}]
type charEncoding = [ | `ascii | `base64 | `binary | `hex | `latin1 | `ucs2 | `utf16le | `utf8];

module Request = {
  [@bs.deriving abstract]
  type t = {
    method: abs_method,
    url: string,
    port: int,
  };

  let method = req => req->methodGet->methodFromJs;

  let path = request =>
    switch (request->urlGet) {
    | ""
    | "/" => []
    | path =>
      /* remove the preceeding /, which every pathname seems to have */
      let raw = Js.String.sliceToEnd(~from=1, path);
      /* remove the trailing /, which some pathnames might have. Ugh */
      let raw =
        switch (Js.String.get(raw, Js.String.length(raw) - 1)) {
        | "/" => Js.String.slice(~from=0, ~to_=-1, raw)
        | _ => raw
        };
      raw |> Js.String.split("/") |> Belt.List.fromArray;
    };

  [@bs.send]
  external on:
    (
      t,
      [@bs.string] [
        | `data(Node.Buffer.t => unit)
        | [@bs.as "end"] `end_(unit => unit)
        | `error(unit => unit)
      ]
    ) =>
    t =
    "";
};

module Response = {
  [@bs.deriving abstract]
  type t = {mutable statusCode: int};

  [@bs.send] external write: (t, string, abs_charEncoding) => unit = "";
  [@bs.send] external setHeader: (t, string, string) => unit = "";
  [@bs.send] external getHeader: (t, string) => string = "";
  [@bs.send] external end_: t => unit = "end";

  let write = (response: t, ~encoding=`utf8, chunk: string) => {
    write(response, chunk, encoding->charEncodingToJs);
    response;
  };

  let setStatusCode = (res, code) => {
    res->statusCodeSet(code);
    res;
  };

  let setStatus = (res, status) => {
    res->statusCodeSet(status);
    res;
  };

  let setHeader = (res, field, value) => {
    res->setHeader(field, value);
    res;
  };

  let appendToHeader = (res, field, value) => {
    let headerValue = res->getHeader(field);
    res->setHeader(headerValue ++ value);
  };
};

module Server = {
  type t;
  [@bs.module "http"] external create: ((Request.t, Response.t) => 'a) => t = "createServer";
  [@bs.send] external listen: (t, ~port: int) => t = "";

  [@bs.send] external on: (t, [@bs.string] [ | `error(unit => unit)]) => t = "";

  let listen = (server, ~port) => {
    Js.log("Server started, listening on port " ++ string_of_int(port));
    listen(server, ~port);
  };
};

[@bs.module "util"] external inspect: 'a => 'b = "";

let introspect = {|
{
    __schema {
    types {
          name
    fields {
      name
      type {
        name
        kind
        ofType {
          name
          kind
        }
      }
    }
    }
  }
}

|};

let server =
  Server.create((req, res) =>
    switch (Request.path(req)) {
    | ["graphql"] =>
      let body = ref("");

      Request.on(req, `data(d => body := body^ ++ Node.Buffer.toString(d)));

      Request.on(
        req,
        `end_(
          () =>
            Schema.execute(
              ~variables=[],
              ~document=Language.Parser.parse(body^),
              ~ctx=(),
              schema,
            )
            ->Future.get(json =>
                Response.setStatus(res, 200)
                ->Response.write(Js.Json.stringify(json->Schema.constValueToJson))
                ->Response.end_
              ),
        ),
      )
      ->ignore;

    | _ => Response.setStatus(res, 404)->Response.write("not found")->Response.end_
    }
  );

Server.listen(server, ~port=3000);
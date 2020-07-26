open Belt.Result;
open GraphqlJsPromise;

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
        abstractField("id", nonnull(int), ~args=[]),
        abstractField("name", nonnull(string), ~args=[]),
        abstractField("appearsIn", nonnull(list(nonnull(episodeEnum.fieldType))), ~args=[]),
        abstractField("friends", nonnull(list(nonnull(character))), ~args=[]),
      ]
    )
  )

and humanAsCharacterInterface =
  lazy(Schema.addType(characterInterface, Lazy.force(humanTypeLazy)))
and droidAsCharacterInterface =
  lazy(Schema.addType(characterInterface, Lazy.force(droidTypeLazy)))
and asCharacterInterface =
  fun
  | StarWars.Human(human) => Lazy.force(humanAsCharacterInterface, human)
  | Droid(droid) => Lazy.force(droidAsCharacterInterface, droid)

and humanTypeLazy =
  lazy(
    Schema.(
      obj("Human", ~description="A humanoid creature in the Star Wars universe.", ~fields=_ =>
        [
          field("id", nonnull(int), ~args=[], ~resolve=(_ctx, human: StarWars.human) => human.id),
          field("name", nonnull(string), ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
            human.StarWars.name
          ),
          field(
            "appearsIn",
            nonnull(list(nonnull(episodeEnum.fieldType))),
            ~args=[],
            ~resolve=(_ctx, human: StarWars.human) =>
            human.StarWars.appearsIn
          ),
          async_field(
            "friends",
            nonnull(list(nonnull(characterInterface))),
            ~args=[],
            ~resolve=(_ctx, human: StarWars.human) =>
            Js.Promise.(
              StarWars.getFriends(human.friends)
              |> then_(list => resolve(Ok(Belt.Array.map(list, asCharacterInterface))))
            )
          ),
          field("homePlanet", string, ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
            human.homePlanet
          ),
        ]
      )
    )
  )

and droidTypeLazy =
  lazy(
    Schema.(
      obj("Droid", ~description="A mechanical creature in the Star Wars universe.", ~fields=_ =>
        [
          field("id", nonnull(int), ~args=[], ~resolve=(_ctx, droid: StarWars.droid) => droid.id),
          field("name", nonnull(string), ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.StarWars.name
          ),
          field(
            "appearsIn",
            nonnull(list(nonnull(episodeEnum.fieldType))),
            ~args=[],
            ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.StarWars.appearsIn
          ),
          field(
            "primaryFunction", nonnull(string), ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.primaryFunction
          ),
          async_field(
            "friends",
            nonnull(list(nonnull(characterInterface))),
            ~args=[],
            ~resolve=(_ctx, droid: StarWars.droid) =>
            Js.Promise.(
              StarWars.getFriends(droid.friends)
              |> then_(list => resolve(Belt.Array.map(list, asCharacterInterface)))
              |> then_(list => resolve(Ok(list)))
            )
          ),
        ]
      )
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
        nonnull(characterInterface),
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
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=(_ctx, (), argId) => {
          let id = argId;
          StarWarsData.getHuman(id) |> Js.Promise.(then_(human => resolve(Ok(human))));
        },
      ),
      async_field(
        "droid",
        droidType,
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=(_ctx, (), argId) => {
          let id = argId;
          StarWarsData.getDroid(id) |> Js.Promise.(then_(human => resolve(Ok(human))));
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
        nonnull(updateCharacterResponse),
        ~args=
          Arg.[
            arg("characterId", nonnull(int)),
            arg("name", nonnull(string)),
            arg("appearsIn", string),
          ],
        ~resolve=(_ctx, (), charId, name, _appearsIn) =>
        StarWarsData.updateCharacterName(charId, name)
        |> Js.Promise.(then_(x => resolve(Ok(x))))
      ),
    ])
  );

let schema: Schema.schema(unit) = Schema.create(query, ~mutation);

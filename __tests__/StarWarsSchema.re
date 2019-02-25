open Belt.Result;

module StarWars = StarWarsData;

module Schema = GraphqlFuture.Schema;

module Future = {
  include Future;
  let ok = x => value(Belt.Result.Ok(x));
};

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
        abstractField("id", int, ~args=[]),
        abstractField("name", string, ~args=[]),
        abstractField("appearsIn", list(episodeEnum.fieldType), ~args=[]),
        abstractField("friends", list(character), ~args=[]),
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
          field("id", int, ~args=[], ~resolve=(_ctx, human: StarWars.human) => human.id),
          field("name", string, ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
            human.StarWars.name
          ),
          field(
            "appearsIn",
            list(episodeEnum.fieldType),
            ~args=[],
            ~resolve=(_ctx, human: StarWars.human) =>
            human.StarWars.appearsIn
          ),
          async_field(
            "friends", list(characterInterface), ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
            StarWars.getFriends(human.friends)
            ->Future.map(list => Belt.List.map(list, asCharacterInterface))
            ->Future.map(list => Ok(list))
          ),
          field("homePlanet", nullable(string), ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
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
          field("id", int, ~args=[], ~resolve=(_ctx, droid: StarWars.droid) => droid.id),
          field("name", string, ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.StarWars.name
          ),
          field(
            "appearsIn",
            list(episodeEnum.fieldType),
            ~args=[],
            ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.StarWars.appearsIn
          ),
          field("primaryFunction", string, ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
            droid.primaryFunction
          ),
          async_field(
            "friends", list(characterInterface), ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
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
        characterInterface,
        ~args=
          Arg.[
            arg(
              "episode",
              nullable(episodeEnum.argType),
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
        nullable(humanType),
        ~args=Arg.[arg("id", int)],
        ~resolve=(_ctx, (), argId) => {
          let id = argId;
          StarWarsData.getHuman(id)->Future.map(human => Ok(human));
        },
      ),
      async_field(
        "droid",
        nullable(droidType),
        ~args=Arg.[arg("id", int)],
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
          nullable(string),
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
          nullable(characterInterface),
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
        updateCharacterResponse,
        ~args=Arg.[arg("characterId", int), arg("name", string)],
        ~resolve=(_ctx, (), charId, name) =>
        StarWarsData.updateCharacterName(charId, name)->Future.flatMap(Future.ok)
      ),
    ])
  );

let schema: Schema.t(unit) = Schema.create(query, ~mutation);
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

let characterInterface: Schema.abstractType('ctx, [ | `Character]) =
  Schema.(
    interface("Character", ~fields=character =>
      [
        abstractField("id", int, ~args=[]),
        abstractField("name", string, ~args=[]),
        abstractField("appearsIn", list(episodeEnum.fieldType), ~args=[]),
        abstractField("friends", list(character), ~args=[]),
      ]
    )
  );

let rec humanTypeLazy =
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
          field(
            "friends", list(characterInterface), ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
            StarWars.getFriends(human.friends)
            |> Belt.List.map(
                 _,
                 fun
                 | Human(human) => humanAsCharacter(human)
                 | Droid(droid) => droidAsCharacter(droid),
               )
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
          field(
            "friends", list(characterInterface), ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
            StarWars.getFriends(droid.friends)
            |> Belt.List.map(
                 _,
                 fun
                 | Human(human) => humanAsCharacter(human)
                 | Droid(droid) => droidAsCharacter(droid),
               )
          ),
        ]
      )
    )

and humanAsCharacter = (h) => Schema.addType(characterInterface, Lazy.force(humanTypeLazy), h)
and droidAsCharacter = (d) => Schema.addType(characterInterface, Lazy.force(droidTypeLazy), d);

let humanType = Lazy.force(humanTypeLazy);
let droidType = Lazy.force(droidTypeLazy);
let queryType =
  Schema.(
    rootQuery([
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
        | Some(EMPIRE) => humanAsCharacter(StarWarsData.luke)
        | _ => droidAsCharacter(StarWarsData.artoo)
        }
      ),
      field(
        "human",
        nullable(humanType),
        ~args=Arg.[arg("id", int)],
        ~resolve=(_ctx, (), argId) => {
          let id = argId;
          StarWarsData.getHuman(id);
        },
      ),
      field(
        "droid",
        nullable(droidType),
        ~args=Arg.[arg("id", int)],
        ~resolve=(_ctx, (), argId) => {
          let id = argId;
          StarWarsData.getDroid(id);
        },
      ),
    ])
  );

let updateCharacterResponse =
  Schema.(
    obj("UpdateCharacterResponse", ~fields=_ =>
      [
        field(
          "character",
          nullable(characterInterface),
          ~args=[],
          ~resolve=(_, updateCharResult: StarWars.updateCharacterResult) =>
          switch (updateCharResult) {
          | Ok(Human(human)) => Some(humanAsCharacter(human))
          | Ok(Droid(droid)) => Some(droidAsCharacter(droid))
          | _ => None
          }
        ),
        field(
          "error",
          nullable(string),
          ~args=[],
          ~resolve=(_, updateCharResult: StarWars.updateCharacterResult) =>
          switch (updateCharResult) {
          | Error(CharacterNotFound(id)) =>
            Some("Character with ID " ++ string_of_int(id) ++ " not found")
          | _ => None
          }
        ),
      ]
    )
  );

let mutationType =
  Schema.(
    rootMutation([
      field(
        "updateCharacterName",
        updateCharacterResponse,
        ~args=Arg.[arg("characterId", int), arg("name", string)],
        ~resolve=(_ctx, (), charId, name) =>
        StarWarsData.updateCharacterName(charId, name)
      ),
    ])
  );

let schema: Schema.t(unit) = Schema.create(~query=queryType, ~mutation=mutationType);
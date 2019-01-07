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
        abstractField("id", ~typ=int, ~args=[]),
        abstractField("name", ~typ=string, ~args=[]),
        abstractField("appearsIn", ~typ=list(episodeEnum.fieldType), ~args=[]),
        abstractField("friends", ~typ=list(character), ~args=[]),
      ]
    )
  );

let humanType =
  Schema.(
    obj("Human", ~description="A humanoid creature in the Star Wars universe.", ~fields=humanType =>
      [
        field("id", ~typ=int, ~args=[], ~resolve=(_ctx, human: StarWars.human) => human.id),
        field("name", ~typ=string, ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
          human.StarWars.name
        ),
        field(
          "appearsIn",
          ~typ=list(episodeEnum.fieldType),
          ~args=[],
          ~resolve=(_ctx, human: StarWars.human) =>
          human.StarWars.appearsIn
        ),
        field("friends", ~typ=list(humanType), ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
          StarWars.getFriends(human.StarWars.friends)
        ),
        field(
          "homePlanet", ~typ=nullable(string), ~args=[], ~resolve=(_ctx, human: StarWars.human) =>
          human.homePlanet
        ),
      ]
    )
  );

let droidType =
  Schema.(
    obj(
      "Droid", ~description="A mechanical creature in the Star Wars universe.", ~fields=_droidType =>
      [
        field("id", ~typ=int, ~args=[], ~resolve=(_ctx, droid: StarWars.droid) => droid.id),
        field("name", ~typ=string, ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
          droid.StarWars.name
        ),
        field(
          "appearsIn",
          ~typ=list(episodeEnum.fieldType),
          ~args=[],
          ~resolve=(_ctx, droid: StarWars.droid) =>
          droid.StarWars.appearsIn
        ),
        field("primaryFunction", ~typ=string, ~args=[], ~resolve=(_ctx, droid: StarWars.droid) =>
          droid.primaryFunction
        ),
      ]
    )
  );

let humanAsCharacter = Schema.addType(characterInterface, humanType);
let droidAsCharacter = Schema.addType(characterInterface, droidType);

let queryType =
  Schema.(
    rootQuery([
      field(
        "hero",
        ~typ=characterInterface,
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
        "human", ~typ=nullable(humanType), ~args=Arg.[arg("id", int)], ~resolve=(_ctx, (), id) =>
        StarWarsData.getHuman(id)
      ),
      field(
        "droid", ~typ=nullable(droidType), ~args=Arg.[arg("id", int)], ~resolve=(_ctx, (), id) =>
        StarWarsData.getDroid(id)
      ),
    ])
  );

let schema: Schema.t(unit) = Schema.create(~query=queryType);
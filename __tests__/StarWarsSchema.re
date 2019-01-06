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

let humanType =
  Schema.(
    obj("Human", ~fields=humanType =>
      [
        field("id", ~typ=string, ~args=[], ~resolve=(human: StarWars.human) => human.id),
        field("name", ~typ=string, ~args=[], ~resolve=(human: StarWars.human) =>
          human.StarWars.name
        ),
        field(
          "appearsIn",
          ~typ=list(episodeEnum.fieldType),
          ~args=[],
          ~resolve=(human: StarWars.human) =>
          human.StarWars.appearsIn
        ),
        field("friends", ~typ=list(humanType), ~args=[], ~resolve=(human: StarWars.human) =>
          StarWars.getFriends(human.StarWars.friends)
        ),
      ]
    )
  );

let queryType =
  Schema.(
    rootQuery([
      field("hero", ~typ=humanType, ~args=Arg.[arg("id", int)], ~resolve=(_, id) =>
        StarWars.getHero(string_of_int(id))
      ),
      field(
        "heroes", ~typ=list(humanType), ~args=Arg.[arg("ids", list(int))], ~resolve=(_, ids) =>
        ids |> List.map(string_of_int) |> StarWars.getFriends
      ),
      field(
        "testDefault",
        ~typ=int,
        ~args=Arg.[defaultArg("number", nullable(int), ~default=5)],
        ~resolve=(_, n) =>
        n
      ),
    ])
  );

let schema: Schema.t = Schema.create(~query=queryType);
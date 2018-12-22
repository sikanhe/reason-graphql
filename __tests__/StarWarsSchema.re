module StarWars = StarWarsData;

let episodeEnum =
  Schema.(
    enum(
      "Episode",
      ~values=[
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
        field("id", string, (human: StarWars.human) => human.id),
        field("name", string, (human: StarWars.human) => human.StarWars.name),
        field("appearsIn", List(episodeEnum), (human: StarWars.human) =>
          human.StarWars.appearsIn
        ),
        field("friends", List(humanType), (human: StarWars.human) =>
          StarWars.getFriends(human.StarWars.friends)
        ),
      ]
    )
  );

let queryType =
  Schema.(
    rootQuery([
      field("hero", humanType, () => StarWars.getHero("1000")),
      field("human", humanType, () => StarWars.getHero("1000")),
    ])
  );

let schema: Schema.t = Schema.create(~query=queryType);
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
        field("id", string, ~args=[], (human: StarWars.human) => human.id),
        field("name", string, ~args=[], (human: StarWars.human) => human.StarWars.name),
        field("appearsIn", ~args=[], List(episodeEnum.fieldType), (human: StarWars.human) =>
          human.StarWars.appearsIn
        ),
        field("friends", ~args=[], List(humanType), (human: StarWars.human) =>
          StarWars.getFriends(human.StarWars.friends)
        ),
      ]
    )
  );

let queryType =
  Schema.(
    rootQuery([
      field("hero", humanType, ~args=[], () => StarWars.getHero("1000")),
      field("human", humanType, ~args=[], () => StarWars.getHero("1000")),
      field("printInt", int,~args=Arg.[arg("int", int)], ((), n) => n),
      field(
        "printEpisode", string, ~args=Arg.[arg("episode", episodeEnum.argType)], ((), episode) =>
        switch (episode) {
        | NEWHOPE => "NEWHOPE"
        | EMPIRE => "EMPIRE"
        | JEDI => "JEDI"
        }
      ),
    ])
  );

let schema: Schema.t = Schema.create(~query=queryType);
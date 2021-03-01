open Belt.Result
open Graphql_Schema

module StarWars = StarWarsData

let episodeEnum = {
  makeEnum(
    "Episode",
    [
      enumValue("NEWHOPE", ~value=StarWars.NEWHOPE, ~description="Released in 1977"),
      enumValue("EMPIRE", ~value=StarWars.EMPIRE, ~description="Released in 1980"),
      enumValue("JEDI", ~value=StarWars.JEDI, ~description="Released in 1983"),
    ],
  )
}

let rec characterInterface: abstractType<'ctx, [#Character]> = interface(
  "Character",
  ~fields=character => [
    abstractField("id", nonnull(int), ~args=Empty),
    abstractField("name", nonnull(string), ~args=Empty),
    abstractField("appearsIn", nonnull(list(nonnull(episodeEnum.fieldType))), ~args=Empty),
    abstractField("friends", nonnull(list(nonnull(character))), ~args=Empty),
  ],
)

and humanAsCharacterInterface = lazy addType(characterInterface, Lazy.force(humanTypeLazy))
and droidAsCharacterInterface = lazy addType(characterInterface, Lazy.force(droidTypeLazy))
and asCharacterInterface = x =>
  switch x {
  | StarWars.Human(human) => Lazy.force(humanAsCharacterInterface, human)
  | Droid(droid) => Lazy.force(droidAsCharacterInterface, droid)
  }

and humanTypeLazy = lazy {
  obj("Human", ~description="A humanoid creature in the Star Wars universe.", ~fields=_ => [
    field("id", nonnull(int), ~args=Empty, ~resolve=(_ctx, human: StarWars.human, _arg) =>
      human.id
    ),
    field("name", nonnull(string), ~args=Empty, ~resolve=(_ctx, human: StarWars.human, _arg) =>
      human.StarWars.name
    ),
    field("appearsIn", nonnull(list(nonnull(episodeEnum.fieldType))), ~args=Empty, ~resolve=(
      _ctx,
      human: StarWars.human,
      _arg,
    ) => human.StarWars.appearsIn),
    asyncField("friends", nonnull(list(nonnull(characterInterface))), ~args=Empty, ~resolve=(
      _ctx,
      human: StarWars.human,
      _arg,
    ) => {
      open Js.Promise
      StarWars.getFriends(human.friends) |> then_(list =>
        resolve(Belt.Array.map(list, asCharacterInterface))
      )
    }),
    field("homePlanet", string, ~args=Empty, ~resolve=(_ctx, human: StarWars.human, _arg) =>
      human.homePlanet
    ),
  ])
}

and droidTypeLazy = lazy {
  obj("Droid", ~description="A mechanical creature in the Star Wars universe.", ~fields=_ => [
    field("id", nonnull(int), ~args=Empty, ~resolve=(_ctx, droid: StarWars.droid, _arg) =>
      droid.StarWars.id
    ),
    field("name", nonnull(string), ~args=Empty, ~resolve=(_ctx, droid: StarWars.droid, _arg) =>
      droid.StarWars.name
    ),
    field("appearsIn", nonnull(list(nonnull(episodeEnum.fieldType))), ~args=Empty, ~resolve=(
      _ctx,
      droid: StarWars.droid,
      _arg,
    ) => droid.StarWars.appearsIn),
    field("primaryFunction", nonnull(string), ~args=Empty, ~resolve=(
      _ctx,
      droid: StarWars.droid,
      _arg,
    ) => droid.primaryFunction),
    asyncField("friends", nonnull(list(nonnull(characterInterface))), ~args=Empty, ~resolve=(
      _ctx,
      droid: StarWars.droid,
      _arg,
    ) => {
      open Js.Promise
      StarWars.getFriends(droid.friends) |> then_(list =>
        resolve(Belt.Array.map(list, asCharacterInterface))
      )
    }),
  ])
}

let humanType = Lazy.force(humanTypeLazy)
let droidType = Lazy.force(droidTypeLazy)
let humanAsCharacterInterface = Lazy.force(humanAsCharacterInterface)
let droidAsCharacterInterface = Lazy.force(droidAsCharacterInterface)

let query = {
  query(list{
    field(
      "hero",
      nonnull(characterInterface),
      ~args={
        open Arg
        Arg1(
          arg(
            "episode",
            episodeEnum.argTyp,
            ~description="If omitted, returns the hero of the whole saga. " ++ "If provided, returns the hero of that particular episode.",
          ),
        )
      },
      ~resolve=(_ctx, (), episode) =>
        switch episode {
        | Some(EMPIRE) => humanAsCharacterInterface(StarWarsData.luke)
        | _ => droidAsCharacterInterface(StarWarsData.artoo)
        },
    ),
    asyncField(
      "human",
      humanType,
      ~args={
        open Arg
        Arg1(arg("id", nonnull(int)))
      },
      ~resolve=(_ctx, (), argId) => {
        StarWarsData.getHuman(argId)
      },
    ),
    asyncField(
      "droid",
      droidType,
      ~args={
        open Arg
        Arg1(arg("id", nonnull(int)))
      },
      ~resolve=(_ctx, (), id) => {
        StarWarsData.getDroid(id)
      },
    ),
  })
}

let updateCharacterResponse = {
  obj("UpdateCharacterResponse", ~fields=_ => [
    field("error", string, ~args=Empty, ~resolve=(
      _,
      updateCharResult: StarWars.updateCharacterResult,
      _arg,
    ) =>
      switch updateCharResult {
      | Ok(_) => None
      | Error(CharacterNotFound(id)) =>
        Some("Character with ID " ++ (string_of_int(id) ++ " not found"))
      }
    ),
    field("character", characterInterface, ~args=Empty, ~resolve=(
      _,
      updateCharResult: StarWars.updateCharacterResult,
      _,
    ) =>
      switch updateCharResult {
      | Ok(char) => Some(char->asCharacterInterface)
      | Error(_) => None
      }
    ),
  ])
}

let mutation = {
  mutation(list{
    asyncField(
      "updateCharacterName",
      nonnull(updateCharacterResponse),
      ~args={
        open Arg
        Arg3(
          arg("characterId", nonnull(int)),
          arg("name", nonnull(string)),
          arg("appearsIn", string),
        )
      },
      ~resolve=(_ctx, (), (charId, name, _appearsIn)) =>
        StarWarsData.updateCharacterName(charId, name),
    ),
  })
}

let schema: schema<unit> = create(query, ~mutation)

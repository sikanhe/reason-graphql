type episode =
  | NEWHOPE
  | EMPIRE
  | JEDI

type human = {
  id: int,
  name: string,
  friends: list<int>,
  appearsIn: array<episode>,
  homePlanet: option<string>,
}

type droid = {
  id: int,
  name: string,
  friends: list<int>,
  appearsIn: array<episode>,
  primaryFunction: string,
}

type character =
  | Human(human)
  | Droid(droid)

let luke = {
  id: 1000,
  name: "Luke Skywalker",
  friends: list{1002, 1003, 2000, 2001},
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  homePlanet: Some("Tatooine"),
}

let vader = {
  id: 1001,
  name: "Darth Vader",
  friends: list{1004},
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  homePlanet: Some("Tatooine"),
}

let han = {
  id: 1002,
  name: "Han Solo",
  friends: list{1000, 1003, 2001},
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  homePlanet: None,
}

let leia = {
  id: 1003,
  name: "Leia Organa",
  friends: list{1000, 1002, 2000, 2001},
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  homePlanet: Some("Alderaan"),
}

let threepio = {
  id: 2000,
  name: "C-3PO",
  friends: list{1000, 1002, 1003, 2001},
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  primaryFunction: "Protocol",
}

let artoo = {
  id: 2001,
  name: "R2-D2",
  friends: list{1000, 1002, 1003},
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  primaryFunction: "Astromech",
}

let getHuman = id =>
  Js.Promise.resolve(Belt.List.getBy(list{luke, han, leia, vader}, human => human.id == id))
let getDroid = id =>
  Js.Promise.resolve(Belt.List.getBy(list{threepio, artoo}, droid => droid.id == id))
let getCharacter = id =>
  getHuman(id) |> {
    open Js.Promise
    then_(x =>
      switch x {
      | Some(human) => resolve(Some(Human(human)))
      | None =>
        getDroid(id) |> then_(x =>
          switch x {
          | Some(droid) => resolve(Some(Droid(droid)))
          | None => resolve(None)
          }
        )
      }
    )
  }

type updateCharacterNameError = CharacterNotFound(int)

type updateCharacterResult = Belt.Result.t<character, updateCharacterNameError>

let updateCharacterName = (id, name): Js.Promise.t<updateCharacterResult> =>
  getCharacter(id) |> {
    open Js.Promise
    then_(x =>
      switch x {
      | Some(Human(human)) => resolve(Belt.Result.Ok(Human({...human, name: name})))
      | Some(Droid(droid)) => resolve(Belt.Result.Ok(Droid({...droid, name: name})))
      | None => resolve(Belt.Result.Error(CharacterNotFound(id)))
      }
    )
  }

let rec futureAll = x =>
  switch x {
  | list{} => Js.Promise.resolve(list{})
  | list{x, ...xs} =>
    Js.Promise.then_(
      xs' => Js.Promise.then_(x' => Js.Promise.resolve(list{x', ...xs'}), x),
      futureAll(xs),
    )
  }

let getFriends = ids =>
  Belt.List.map(ids, id =>
    id
    |> getCharacter
    |> {
      open Js.Promise
      then_(x => resolve(Belt.Option.getExn(x)))
    }
  )
  |> Array.of_list
  |> Js.Promise.all

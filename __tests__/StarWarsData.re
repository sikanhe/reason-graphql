type episode =
  | NEWHOPE
  | EMPIRE
  | JEDI;

type human = {
  id: int,
  name: string,
  friends: list(int),
  appearsIn: list(episode),
  homePlanet: option(string),
};

type droid = {
  id: int,
  name: string,
  friends: list(int),
  appearsIn: list(episode),
  primaryFunction: string,
};

type character =
  | Human(human)
  | Droid(droid);

let luke = {
  id: 1000,
  name: "Luke Skywalker",
  friends: [1002, 1003, 2000, 2001],
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  homePlanet: Some("Tatooine"),
};

let vader = {
  id: 1001,
  name: "Darth Vader",
  friends: [1004],
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  homePlanet: Some("Tatooine"),
};

let han = {
  id: 1002,
  name: "Han Solo",
  friends: [1000, 1003, 2001],
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  homePlanet: None,
};

let leia = {
  id: 1003,
  name: "Leia Organa",
  friends: [1000, 1002, 2000, 2001],
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  homePlanet: Some("Alderaan"),
};

let threepio = {
  id: 2000,
  name: "C-3PO",
  friends: [1000, 1002, 1003, 2001],
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  primaryFunction: "Protocol",
};

let artoo = {
  id: 2001,
  name: "R2-D2",
  friends: [1000, 1002, 1003],
  appearsIn: [NEWHOPE, EMPIRE, JEDI],
  primaryFunction: "Astromech",
};

let getHuman = id =>
  Future.value(Belt.List.getBy([luke, han, leia, vader], human => human.id == id));
let getDroid = id => Future.value(Belt.List.getBy([threepio, artoo], droid => droid.id == id));
let getCharacter = id => {
  getHuman(id)
  ->Future.flatMap(
      fun
      | Some(human) => Future.value(Some(Human(human)))
      | None => {
          getDroid(id)
          ->Future.map(
              fun
              | Some(droid) => Some(Droid(droid))
              | None => None,
            );
        },
    );
};
type updateCharacterNameError =
  | CharacterNotFound(int);

type updateCharacterResult = Belt.Result.t(character, updateCharacterNameError);

let updateCharacterName = (id, name): Future.t(updateCharacterResult) => {
  getCharacter(id)
  ->Future.map(
      fun
      | Some(Human(human)) => Belt.Result.Ok(Human({...human, name}))
      | Some(Droid(droid)) => Belt.Result.Ok(Droid({...droid, name}))
      | None => Belt.Result.Error(CharacterNotFound(id)),
    );
};

let rec futureAll = fun 
    | [] => Future.value([])
    | [x, ...xs] => Future.flatMap(futureAll(xs), xs' => Future.map(x, x' => [x', ...xs']));

let getFriends = ids => {
  Belt.List.map(ids, id => {
    id -> getCharacter -> Future.map(Belt.Option.getExn)
  })
  |> futureAll
};
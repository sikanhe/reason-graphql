type episode =
  | NEWHOPE
  | EMPIRE
  | JEDI;

type human = {
  id: int,
  mutable name: string,
  friends: list(int),
  appearsIn: list(episode),
  homePlanet: option(string),
};

type droid = {
  id: int,
  mutable name: string,
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
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
  homePlanet: Some("Tatooine"),
};

let vader = {
  id: 1001,
  name: "Darth Vader",
  friends: [1004],
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
  homePlanet: Some("Tatooine"),
};

let han = {
  id: 1002,
  name: "Han Solo",
  friends: [1000, 1003, 2001],
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
  homePlanet: None,
};

let leia = {
  id: 1003,
  name: "Leia Organa",
  friends: [1000, 1002, 2000, 2001],
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
  homePlanet: Some("Alderaan"),
};

let threepio = {
  id: 2000,
  name: "C-3PO",
  friends: [1000, 1002, 1003, 2001],
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
  primaryFunction: "Protocol",
};

let artoo = {
  id: 2001,
  name: "R2-D2",
  friends: [1000, 1002, 1003],
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
  primaryFunction: "Astromech",
};

let getHuman = id => Belt.List.getBy([luke, han, leia, vader], human => human.id == id);
let getDroid = id => Belt.List.getBy([threepio, artoo], droid => droid.id == id);
let getCharacter = id => {
  switch (getHuman(id)) {
  | Some(human) => Some(Human(human))
  | None =>
    switch (getDroid(id)) {
    | Some(droid) => Some(Droid(droid))
    | None => None
    }
  };
};

type updateCharacterNameError = CharacterNotFound(int);
type result('a, 'b) = Ok('a) | Error('b);

type updateCharacterResult = result(character, updateCharacterNameError);

let updateCharacterName = (id, name): updateCharacterResult => {
  switch (getCharacter(id)) {
  | Some(Human(human)) =>
    human.name = name;
    Ok(Human(human));
  | Some(Droid(droid)) =>
    droid.name = name;
    Ok(Droid(droid));
  | None => Error(CharacterNotFound(id))
  };
};

let getFriends = ids => List.map(id => getHuman(id) |> Belt.Option.getExn, ids);
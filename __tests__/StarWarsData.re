type episode =
  | NEWHOPE
  | EMPIRE
  | JEDI;

type human = {
  id: string,
  name: string,
  friends: list(string),
  appearsIn: list(episode),
};

type droid = {
  id: string,
  name: string,
  friends: list(string),
  appearsIn: list(episode),
  primaryFunction: string,
};

type character =
  | Human(human)
  | Droid(droid);

let luke = {
  id: "1000",
  name: "Luke Skywalker",
  friends: ["1002", "1003", "2000", "2001"],
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
  /* homePlanet: "Tatooine", */
};

let han = {
  id: "1002",
  name: "Han Solo",
  friends: ["1000", "1003", "2001"],
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
};

let leia = {
  id: "1003",
  name: "Leia Organa",
  friends: ["1000", "1002", "2000", "2001"],
  appearsIn: [NEWHOPE, JEDI, EMPIRE],
};

let threepio =
  Droid({
    id: "2000",
    name: "C-3PO",
    friends: ["1000", "1002", "1003", "2001"],
    appearsIn: [NEWHOPE, JEDI, EMPIRE],
    primaryFunction: "Protocol",
  });

let artoo =
  Droid({
    id: "2001",
    name: "R2-D2",
    friends: ["1000", "1002", "1003"],
    appearsIn: [NEWHOPE, JEDI, EMPIRE],
    primaryFunction: "Astromech",
  });

let characters = [luke, han, leia];

let getHero = (id: string): human => List.find((human: human) => human.id == id, characters);

let getFriends = (ids: list(string)) => List.map(id => getHero(id), ids);
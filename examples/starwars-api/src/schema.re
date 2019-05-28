open GraphqlFuture;


let starship =
  Schema.(
    obj("starship", ~fields=_ =>
      [
        field(
          "name",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.name
        ),
        field(
          "model",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.model
        ),
        field(
          "starshipClass",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.starshipClass
        ),
        field(
          "manufacturers",
          nonnull(list(nonnull(string))),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.manufacturers
        ),
        field(
          "costInCredits",
          float,
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.costInCredits
        ),
        field(
          "length",
          nonnull(float),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.length
        ),
        field(
          "crew",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.crew
        ),
        field(
          "passengers",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.passengers
        ),
        field(
          "maxAtmospheringSpeed",
          int,
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.maxAtmospheringSpeed
        ),
        field(
          "hyperdriveRating",
          nonnull(float),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.hyperdriveRating
        ),
        field(
          "mglt",
          nonnull(int),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.mglt
        ),
        field(
          "cargoCapacity",
          nonnull(float),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.cargoCapacity
        ),
        field(
          "consumables",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), starship: Swapi.starship) =>
          starship.consumables
        ),
      ]
    )
  );

let rec film =
  lazy
    Schema.(
      obj("film", ~fields=_ =>
        [
          field(
            "title",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), film: Swapi.film) =>
            film.title
          ),
          field(
            "episodeID",
            nonnull(int),
            ~args=Arg.[],
            ~resolve=((), film: Swapi.film) =>
            film.episodeID
          ),
          field(
            "openingCrawl",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), film: Swapi.film) =>
            film.openingCrawl
          ),
          field(
            "director",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), film: Swapi.film) =>
            film.director
          ),
          field(
            "producers",
            nonnull(list(nonnull(string))),
            ~args=Arg.[],
            ~resolve=((), film: Swapi.film) =>
            film.producers
          ),
          field(
            "releaseDate",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), film: Swapi.film) =>
            film.releaseDate
          ),
          async_field(
            "characters",
            nonnull(list(nonnull(Lazy.force(person)))),
            ~args=Arg.[],
            ~resolve=((), film: Swapi.film) =>
            Swapi.getEntitiesByUrls(Swapi.decodePerson, film.characterUrls)
          ),
        ]
      )
    )

and person =
  lazy
    Schema.(
      obj("person", ~fields=_ =>
        [
          field(
            "name",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), person: Swapi.person) =>
            person.name
          ),
          field(
            "birthYear",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), person: Swapi.person) =>
            person.birthYear
          ),
          field(
            "eyeColor",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), person: Swapi.person) =>
            person.eyeColor
          ),
          field(
            "gender",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), person: Swapi.person) =>
            person.gender
          ),
          field(
            "hairColor",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), person: Swapi.person) =>
            person.hairColor
          ),
          field(
            "height", int, ~args=Arg.[], ~resolve=((), person: Swapi.person) =>
            person.height
          ),
          field(
            "mass", float, ~args=Arg.[], ~resolve=((), person: Swapi.person) =>
            person.mass
          ),
          field(
            "skinColor",
            nonnull(string),
            ~args=Arg.[],
            ~resolve=((), person: Swapi.person) =>
            person.skinColor
          ),
          async_field(
            "films",
            nonnull(list(nonnull(Lazy.force(film)))),
            ~args=Arg.[],
            ~resolve=((), person: Swapi.person) =>
            Swapi.getEntitiesByUrls(Swapi.decodeFilm, person.filmUrls)
          ),
          async_field(
            "starships",
            nonnull(list(nonnull(starship))),
            ~args=Arg.[],
            ~resolve=((), person: Swapi.person) =>
            Swapi.getEntitiesByUrls(Swapi.decodeStarship, person.starshipUrls)
          ),
        ]
      )
    );

let planet =
  Schema.(
    obj("planet", ~fields=_ =>
      [
        field(
          "name",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.name
        ),
        field(
          "diameter",
          nonnull(int),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.diameter
        ),
        field(
          "rotationPeriod",
          nonnull(int),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.rotationPeriod
        ),
        field(
          "orbitalPeriod",
          nonnull(int),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.orbitalPeriod
        ),
        field(
          "gravity",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.gravity
        ),
        field(
          "population",
          nonnull(float),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.population
        ),
        field(
          "climates",
          nonnull(list(nonnull(string))),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.climates
        ),
        field(
          "terrains",
          nonnull(list(nonnull(string))),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.terrains
        ),
        field(
          "surfaceWater",
          nonnull(float),
          ~args=Arg.[],
          ~resolve=((), planet: Swapi.planet) =>
          planet.surfaceWater
        ),
      ]
    )
  );

let species =
  Schema.(
    obj("species", ~fields=_ =>
      [
        field(
          "name",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.name
        ),
        field(
          "classification",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.classification
        ),
        field(
          "designation",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.designation
        ),
        field(
          "averageHeight",
          nonnull(float),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.averageHeight
        ),
        field(
          "averageLifespan",
          nonnull(int),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.averageLifespan
        ),
        field(
          "eyeColors",
          nonnull(list(nonnull(string))),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.eyeColors
        ),
        field(
          "hairColors",
          nonnull(list(nonnull(string))),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.hairColors
        ),
        field(
          "skinColors",
          nonnull(list(nonnull(string))),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.skinColors
        ),
        field(
          "language",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), species: Swapi.species) =>
          species.language
        ),
      ]
    )
  );

let vehicle =
  Schema.(
    obj("vehicle", ~fields=_ =>
      [
        field(
          "name",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.name
        ),
        field(
          "model",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.model
        ),
        field(
          "starshipClass",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.vehicleClass
        ),
        field(
          "manufacturers",
          nonnull(list(nonnull(string))),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.manufacturers
        ),
        field(
          "costInCredits",
          float,
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.costInCredits
        ),
        field(
          "length",
          nonnull(float),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.length
        ),
        field(
          "crew",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.crew
        ),
        field(
          "passengers",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.passengers
        ),
        field(
          "maxAtmospheringSpeed",
          int,
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.maxAtmospheringSpeed
        ),
        field(
          "cargoCapacity",
          nonnull(float),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.cargoCapacity
        ),
        field(
          "consumables",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), vehicle: Swapi.vehicle) =>
          vehicle.consumables
        ),
      ]
    )
  );

Lazy.force(film);
Lazy.force(person);

let rootQuery =
  Schema.(
    query([
      async_field(
        "allFilms",
        nonnull(list(nonnull(Lazy.force(film)))),
        ~args=Arg.[],
        ~resolve=((), ()) =>
        Swapi.getAllFilms()
      ),
      async_field(
        "film",
        Lazy.force(film),
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=((), (), id) =>
        Swapi.getFilm(id)
      ),
      async_field(
        "allPeople",
        nonnull(list(nonnull(Lazy.force(person)))),
        ~args=Arg.[],
        ~resolve=((), ()) =>
        Swapi.getAllPeople()
      ),
      async_field(
        "person",
        Lazy.force(person),
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=((), (), id) =>
        Swapi.getPerson(id)
      ),
      async_field(
        "planet",
        planet,
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=((), (), id) =>
        Swapi.getPlanet(id)
      ),
      async_field(
        "species",
        species,
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=((), (), id) =>
        Swapi.getSpecies(id)
      ),
      async_field(
        "starship",
        starship,
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=((), (), id) =>
        Swapi.getStarship(id)
      ),
      async_field(
        "vehicle",
        vehicle,
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=((), (), id) =>
        Swapi.getVehicle(id)
      ),
    ])
  );

let schema = Schema.create(rootQuery);
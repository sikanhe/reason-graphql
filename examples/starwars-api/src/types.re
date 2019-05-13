type film = {
  title: string,
  episodeID: int,
  openingCrawl: string,
  director: string,
  producers: list(string),
  releaseDate: string
}

type person = {
  name: string,
  birthYear: string,
  eyeColor: string, 
  gender: string, 
  hairColor: string,
  height: int,
  mass: float,
  skinColor: string,
}

type planet = {
  name: string, 
  diameter: int, 
  rotationPeriod: int,
  orrbitalPeriod: int, 
  gravity: string, 
  population: float,
  climates: list(string),
  terrains: list(string),
  surfaceWater: float
}

type species = {
  name: string,
  classification: string,
  designation: string,
  averageHeight: float,
  averageLifespan: int,
  eyeColors: list(string),
  hairColors: list(string),
  skinColors: list(string),
  language: string
}

type starship = {
  name: string,
  model: string, 
  starshipClass: string,
  manufacturers: list(string),
  costInCredits: float,
  length: float,
  crew: string,
  passengers: string,
  maxAtmospheringSpeed: int,
  hyperdriveRating: float,
  mglt: int,
  cargoCapacity: float,
  consumables: string
}

type vehicle = {
  name: string,
  model: string,
  vehicleClass: string,
  manufacturers: list(string),
  costInCredits: float,
  length: float,
  crew: string,
  passengers: list(string),
  maxAtmospheringSpeed: int,
  cargoCapacity: float,
  consumables: string
}


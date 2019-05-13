open GraphqlFuture;

let film =
  Schema.(
    obj("film", ~fields=_ =>
      [
        field(
          "title",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), film: Types.film) =>
          film.title
        ),
        field(
          "episodeID",
          nonnull(int),
          ~args=Arg.[],
          ~resolve=((), film: Types.film) =>
          film.episodeID
        ),
        field(
          "openingCrawl",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), film: Types.film) =>
          film.openingCrawl
        ),
        field(
          "director",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), film: Types.film) =>
          film.director
        ),
        field(
          "producers",
          nonnull(list(nonnull(string))),
          ~args=Arg.[],
          ~resolve=((), film: Types.film) =>
          film.producers
        ),
        field(
          "releaseDate",
          nonnull(string),
          ~args=Arg.[],
          ~resolve=((), film: Types.film) =>
          film.releaseDate
        ),
      ]
    )
  );

let rootQuery =
  Schema.(
    query([
      async_field(
        "film",
        film,
        ~args=Arg.[arg("id", nonnull(int))],
        ~resolve=((), (), id) =>
        Api.getFilm(id)
      ),
    ])
  );

let schema = Schema.create(rootQuery);
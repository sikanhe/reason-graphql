module Result = {
  include Belt.Result;
  let let_ = flatMap;
};

type result('a) = Result.t('a, GraphqlLanguageError.t);
let syntaxError = a => Result.Error(GraphqlLanguageError.SyntaxError(a));

type token =
  | StartOfFile
  | EndOfFile
  | Bang
  | Dollar
  | Amp
  | ParenOpen
  | ParenClose
  | Spread
  | Colon
  | Equals
  | At
  | BracketOpen
  | BracketClose
  | BraceOpen
  | BraceClose
  | Pipe
  | Name(string)
  | Int(string)
  | Float(string)
  | String(string)
  | Comment(string);

let tokenKind =
  fun
  | StartOfFile => "<SOF>"
  | EndOfFile => "<EOF>"
  | Bang => "!"
  | Dollar => "$"
  | Amp => "$"
  | ParenOpen => "("
  | ParenClose => ")"
  | Spread => "..."
  | Colon => ":"
  | Equals => "="
  | At => "@"
  | BracketOpen => "["
  | BracketClose => "]"
  | BraceOpen => "{"
  | BraceClose => "}"
  | Pipe => "|"
  | Name(_) => "Name"
  | Int(_) => "Int"
  | Float(_) => "Float"
  | String(_) => "String"
  | Comment(_) => "Comment";

type location = {
  start: int,
  end_: int,
  line: int,
  column: int,
};

type t = {
  body: string,
  length: int,
  // mutable lastToken: token,
  mutable token: (token, location),
  mutable line: int,
  mutable lineStart: int,
};

/* (line:col) kind: <kind>, value: <value> */
let printToken = ((token: token, loc: location)) => {
  let locationDescription =
    "(" ++ string_of_int(loc.line) ++ ":" ++ string_of_int(loc.column) ++ ")";

  let tokenDescription =
    switch (token) {
    | Name(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | Int(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | Float(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | String(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | Comment(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | token => " kind: '" ++ tokenKind(token) ++ "'"
    };

  locationDescription ++ tokenDescription;
};

let isChar = (body, position, char) => position < String.length(body) && body.[position] == char;

/**
 * Reads from body starting at startPosition until it finds a non-whitespace
 * character, then returns the position of that character for lexing.
 */
let positionAfterWhitespace = (lexer, startPosition) => {
  let {body, length} = lexer;

  let rec aux = position =>
    if (position >= length) {
      position;
    } else {
      switch (body.[position]) {
      | bom when int_of_char(bom) == 0xFEFF => position + 1
      | ' '
      | ','
      | '\t' => aux(position + 1)
      | '\n' =>
        let newPosition = position + 1;
        lexer.line = lexer.line + 1;
        lexer.lineStart = newPosition;
        aux(newPosition);
      | '\r' =>
        let newPosition = isChar(body, position + 1, '\n') ? position + 2 : position + 1;

        lexer.line = lexer.line + 1;
        lexer.lineStart = newPosition;
        aux(newPosition);
      | _ => position
      };
    };

  aux(startPosition);
};

let isNameChar =
  fun
  | 'A'..'Z'
  | 'a'..'z'
  | '_' => true
  | _ => false;

/**
 * Reads an alphanumeric + underscore name from the source.
 *
 * [_A-Za-z][_0-9A-Za-z]*
 */
let readName = (body, start, line, column): (token, location) => {
  let rec aux = position =>
    switch (body.[position]) {
    | 'A'..'Z'
    | 'a'..'z'
    | '_' => aux(position + 1)
    | _ =>
      let tok = Name(String.sub(body, start, position - start));
      let loc = {start, line, end_: position, column};

      (tok, loc);
    };

  aux(start);
};

/**
 * Reads a comment token from the source file.
 *
 * #[\u0009\u0020-\uFFFF]*
 */
let readComment = (body, start, line, column): (token, location) => {
  let rec aux = position =>
    if (position > String.length(body)) {
      position;
    } else {
      switch (body.[position]) {
      | '\r'
      | '\n' => position
      | _ => aux(position + 1)
      };
    };

  let position = aux(start);
  let loc = {start, line, end_: position, column};
  let tok = Comment(String.sub(body, start, position - start));
  (tok, loc);
};

let readDigits = (body, startingPosition): result(int) => {
  let rec aux = (body, pos) =>
    if (pos >= String.length(body)) {
      Result.Ok(pos);
    } else {
      switch (body.[pos]) {
      | '0'..'9' => aux(body, pos + 1)
      | c when pos === startingPosition =>
        syntaxError("Invalid number, expected digit but got: " ++ String.make(1, c))
      | _ => Ok(pos)
      };
    };

  aux(body, startingPosition);
};

/**
 * Reads a number token from the source file, either a float
 * or an int depending on whether a decimal point appears.
 *
 * Int:   -?(0|[1-9][0-9]*)
 * Float: -?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(+|-)?[0-9]+)?
 */
let readNumber = (body, start, line, column): result((token, location)) => {
  let isInt = ref(true);
  let position = ref(start);

  if (body.[start] == '-') {
    position := position^ + 1;
  };

  let%Result () =
    if (body.[position^] == '0') {
      position := position^ + 1;
      switch (body.[position^]) {
      | '0'..'9' as char =>
        syntaxError("Invalid number, unexpected digit after 0: " ++ String.make(1, char))
      | _ => Ok()
      };
    } else {
      let%Result pos = readDigits(body, position^);
      Ok(position := pos);
    };

  let%Result () =
    if (isChar(body, position^, '.')) {
      isInt := false;
      position := position^ + 1;
      let%Result pos = readDigits(body, position^);
      Ok(position := pos);
    } else {
      Ok();
    };

  let%Result () =
    if (isChar(body, position^, 'E') || isChar(body, position^, 'e')) {
      isInt := false;
      position := position^ + 1;

      if (isChar(body, position^, '+') || isChar(body, position^, '-')) {
        position := position^ + 1;
      };

      let%Result pos = readDigits(body, position^);
      Ok(position := pos);
    } else {
      Ok();
    };

  let loc = {start, end_: position^, line, column};

  let tok =
    isInt^ ?
      Int(String.sub(body, start, position^ - start)) :
      Float(String.sub(body, start, position^ - start));

  Ok((tok, loc));
};

/**
 * Converts a hex character to its integer value.
 * '0' becomes 0, '9' becomes 9
 * 'A' becomes 10, 'F' becomes 15
 * 'a' becomes 10, 'f' becomes 15
 *
 * Returns -1 on error.
 */
let char2hex = c =>
  c >= 48 && c <= 57 ?
    c - 48 : // 0-9
    c >= 65 && c <= 70 ?
      c - 55 : // c-F
      c >= 97 && c <= 102 ?
        c - 87 : // a-f
        (-1);

/**
 * Converts four hexadecimal chars to the integer that the
 * string represents. For example, uniCharCode('0','0','0','f')
 * will return 15, and uniCharCode('0','0','f','f') returns 255.
 *
 * Returns a negative number on error, if a char was invalid.
 *
 * This is implemented by noting that char2hex() returns -1 on error,
 * which means the result of ORing the char2hex() will also be negative.
 */
let uniCharCode = (a, b, c, d) =>
  char2hex(a) lsl 12 lor char2hex(b) lsl 8 lor char2hex(c) lsl 4 lor char2hex(d);

/**
 * Reads a string token from the source file.
 *
 * "([^"\\\u000A\u000D]|(\\(u[0-9a-fA-F]{4}|["\\/bfnrt])))*"
 */
let readString = (body, start, line, column): result((token, location)) => {
  let rec aux = (value, position, chunkStart) => {
    let%Result () =
      if (position >= String.length(body)) {
        syntaxError("Unterminated string");
      } else {
        Ok();
      };

    switch (body.[position]) {
    | '\n' => syntaxError("Unterminated string")
    | '"' =>
      let loc = {line, column, start, end_: position + 1};
      let tok = String(value ++ String.sub(body, chunkStart, position - chunkStart));
      Ok((tok, loc));
    | c when Char.code(c) == 92 =>
      let newPosition = ref(position + 1);
      let code = body.[newPosition^]->Char.code;
      let%Result rest =
        switch (code) {
        | 34 => Ok("\"")
        | 47 => Ok("/")
        | 92 => Ok("\\")
        | 98 => Ok("\b")
        | 102 => Ok("\\f")
        | 110 => Ok("\n")
        | 114 => Ok("\r")
        | 116 => Ok("\t")
        // u
        | 117 =>
          let charCode =
            uniCharCode(
              body.[position + 1]->Char.code,
              body.[position + 2]->Char.code,
              body.[position + 3]->Char.code,
              body.[position + 4]->Char.code,
            );

          if (charCode < 0) {
            syntaxError(
              "Invalid character escape sequence: "
              ++ "\\u"
              ++ String.sub(body, position + 1, position + 5)
              ++ ".",
            );
          } else {
            newPosition := newPosition^ + 4;
            Ok(Char.chr(charCode) |> String.make(1));
          };

        | _ =>
          syntaxError(
            "Invalid character escape sequence: \\" ++ (Char.chr(code) |> String.make(1)),
          )
        };
      let value = value ++ String.sub(body, chunkStart, newPosition^ - chunkStart - 1) ++ rest;

      let nextPosition = newPosition^ + 1;
      aux(value, nextPosition, nextPosition);
    | _ => aux(value, position + 1, chunkStart)
    };
  };

  aux("", start + 1, start + 1);
};

/**
 * Gets the next token from the source starting at the given position.
 *
 * This skips over whitespace until it finds the next lexable token, then lexes
 * punctuators immediately or calls the appropriate helper function for more
 * complicated tokens.
 */
let readToken = (lexer, (prevToken, prevTokenLocation)): result((token, location)) => {
  let {body, length} = lexer;
  let position = positionAfterWhitespace(lexer, prevTokenLocation.end_);
  let line = lexer.line;
  let column = 1 + position - lexer.lineStart;

  // let prev = Some(prevToken);

  let location = {start: position, end_: position + 1, line, column};

  if (position >= length) {
    Ok((EndOfFile, location));
  } else {
    switch (body.[position]) {
    | '!' => Ok((Bang, location))
    | '$' => Ok((Dollar, location))
    | '&' => Ok((Amp, location))
    | '(' => Ok((ParenOpen, location))
    | ')' => Ok((ParenClose, location))
    | ':' => Ok((Colon, location))
    | '=' => Ok((Equals, location))
    | '@' => Ok((At, location))
    | '[' => Ok((BracketOpen, location))
    | ']' => Ok((BracketClose, location))
    | '{' => Ok((BraceOpen, location))
    | '|' => Ok((Pipe, location))
    | '}' => Ok((BraceClose, location))
    | '.' =>
      if (isChar(body, position + 1, '.') && isChar(body, position + 2, '.')) {
        let loc = {...location, end_: position + 3};

        Ok((Spread, loc));
      } else if (position + 1 >= String.length(body)) {
        syntaxError("Unexpected End of File");
      } else {
        syntaxError("Unexpected Character" ++ String.make(1, body.[position + 1]));
      }
    | 'A'..'Z'
    | 'a'..'z'
    | '_' => Ok(readName(body, position, line, column))
    | '0'..'9'
    | '-' => readNumber(body, position, line, column)
    | '"' => readString(body, position, line, column)
    | '#' => Ok(readComment(body, position, line, column))
    | char => syntaxError("Unexpected Character" ++ String.make(1, char))
    };
  };
};

let sof = (StartOfFile, {start: 0, end_: 0, column: 0, line: 1});

let make = body => {body, length: String.length(body), token: sof, line: 1, lineStart: 0};

let lookahead =
  fun
  | {token: (EndOfFile, _) as token} => Result.Ok(token)
  | lexer => {
      let rec skipComment = token => {
        let%Result (currToken', loc) = readToken(lexer, token);
        switch (currToken') {
        | Comment(_) => skipComment((currToken', loc))
        | _ => Ok((currToken', loc))
        };
      };

      skipComment(lexer.token);
    };

let advance = lexer => {
  // lexer.lastToken = lexer.token;
  let%Result token = lookahead(lexer);
  lexer.token = token;
  Ok(token);
};
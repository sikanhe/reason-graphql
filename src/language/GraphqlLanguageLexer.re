module Result = {
  include Belt.Result;
  let let_ = flatMap;
};

type result('a) = Result.t('a, GraphqlLanguageError.t);
let syntaxError = a => Result.Error(GraphqlLanguageError.SyntaxError(a));

type location = {
  start: int,
  end_: int,
  line: int,
  column: int,
};

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

type tokenResult = {
  token,
  location,
};

type t = {
  source: string,
  mutable curr: tokenResult,
  mutable line: int,
  mutable lineStart: int,
};

/* (line:col) kind: <kind>, value: <value> */
let tokenDesc = ({token, location}) => {
  "("
  ++ string_of_int(location.line)
  ++ ":"
  ++ string_of_int(location.column)
  ++ ")"
  ++ (
    switch (token) {
    | Name(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | Int(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | Float(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | String(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | Comment(v) => " kind: '" ++ tokenKind(token) ++ "', value: " ++ v
    | token => " kind: '" ++ tokenKind(token) ++ "'"
    }
  );
};

let isChar = (source, position, char) =>
  position < String.length(source) && source.[position] == char;

/**
 * Reads from source starting at startPosition until it finds a non-whitespace
 * character, then returns the position of that character for lexing.
 */
let positionAfterWhitespace = (lexer, startPosition) => {
  let {source} = lexer;

  let rec aux = position =>
    if (position >= String.length(source)) {
      position;
    } else {
      switch (source.[position]) {
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
        let newPosition = isChar(source, position + 1, '\n') ? position + 2 : position + 1;

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
let readName = (source, ~start, ~line, ~column): tokenResult => {
  let rec aux = position =>
    switch (source.[position]) {
    | 'A'..'Z'
    | 'a'..'z'
    | '_' => aux(position + 1)
    | _ => {
        token: Name(String.sub(source, start, position - start)),
        location: {
          start,
          line,
          end_: position,
          column,
        },
      }
    };

  aux(start);
};

/**
 * #[\u0009\u0020-\uFFFF]*
 */
let readComment = (source, ~start, ~line, ~column): tokenResult => {
  let rec aux = position =>
    if (position > String.length(source)) {
      position;
    } else {
      switch (source.[position]) {
      | '\r'
      | '\n' => position
      | _ => aux(position + 1)
      };
    };

  let position = aux(start);

  {
    token: Comment(String.sub(source, start, position - start)),
    location: {
      start,
      line,
      end_: position,
      column,
    },
  };
};

let readDigits = (source, startingPosition): result(int) => {
  let rec aux = (source, pos) =>
    if (pos >= String.length(source)) {
      Result.Ok(pos);
    } else {
      switch (source.[pos]) {
      | '0'..'9' => aux(source, pos + 1)
      | c when pos === startingPosition =>
        syntaxError("Invalid number, expected digit but got: " ++ String.make(1, c))
      | _ => Ok(pos)
      };
    };

  aux(source, startingPosition);
};

/**
 * Reads a number token from the source file, either a float
 * or an int depending on whether a decimal point appears.
 *
 * Int:   -?(0|[1-9][0-9]*)
 * Float: -?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(+|-)?[0-9]+)?
 */
let readNumber = (source, ~start, ~line, ~column): result(tokenResult) => {
  let isFloat = ref(false);
  let position = ref(start);

  if (source.[start] == '-') {
    position := position^ + 1;
  };

  let%Result () =
    if (source.[position^] == '0') {
      position := position^ + 1;
      switch (source.[position^]) {
      | '0'..'9' as char =>
        syntaxError("Invalid number, unexpected digit after 0: " ++ String.make(1, char))
      | _ => Ok()
      };
    } else {
      let%Result pos = readDigits(source, position^);
      Ok(position := pos);
    };

  let%Result () =
    if (isChar(source, position^, '.')) {
      isFloat := true;
      position := position^ + 1;
      let%Result pos = readDigits(source, position^);
      Ok(position := pos);
    } else {
      Ok();
    };

  let%Result () =
    if (isChar(source, position^, 'E') || isChar(source, position^, 'e')) {
      isFloat := true;
      position := position^ + 1;

      if (isChar(source, position^, '+') || isChar(source, position^, '-')) {
        position := position^ + 1;
      };

      let%Result pos = readDigits(source, position^);
      Ok(position := pos);
    } else {
      Ok();
    };

  let loc = {start, end_: position^, line, column};

  let tok =
    isFloat^
      ? Float(String.sub(source, start, position^ - start))
      : Int(String.sub(source, start, position^ - start));

  Ok({token: tok, location: loc});
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
  c >= 48 && c <= 57
    ? c - 48  // 0-9
    : c >= 65 && c <= 70
        ? c - 55  // c-F
        : c >= 97 && c <= 102
            ? c - 87  // a-f
            : (-1);

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
let readString = (source, ~start, ~line, ~column): result(tokenResult) => {
  let rec aux = (value, position, chunkStart) => {
    let%Result () =
      if (position >= String.length(source)) {
        syntaxError("Unterminated string");
      } else {
        Ok();
      };

    switch (source.[position]) {
    | '\n' => syntaxError("Unterminated string")
    | '"' =>
      Ok({
        token: String(value ++ String.sub(source, chunkStart, position - chunkStart)),
        location: {
          line,
          column,
          start,
          end_: position + 1,
        },
      })
    | c when Char.code(c) == 92 =>
      let newPosition = ref(position + 1);
      let code = source.[newPosition^]->Char.code;
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
              source.[position + 1]->Char.code,
              source.[position + 2]->Char.code,
              source.[position + 3]->Char.code,
              source.[position + 4]->Char.code,
            );

          if (charCode < 0) {
            syntaxError(
              "Invalid character escape sequence: "
              ++ "\\u"
              ++ String.sub(source, position + 1, position + 5)
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
      let value = value ++ String.sub(source, chunkStart, newPosition^ - chunkStart - 1) ++ rest;

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
let readToken = (lexer, prevToken): result(tokenResult) => {
  let {source} = lexer;
  let position = positionAfterWhitespace(lexer, prevToken.location.end_);
  let line = lexer.line;
  let column = 1 + position - lexer.lineStart;

  let location = {start: position, end_: position + 1, line, column};

  if (position >= String.length(source)) {
    Ok({token: EndOfFile, location});
  } else {
    switch (source.[position]) {
    | '!' => Ok({token: Bang, location})
    | '$' => Ok({token: Dollar, location})
    | '&' => Ok({token: Amp, location})
    | '(' => Ok({token: ParenOpen, location})
    | ')' => Ok({token: ParenClose, location})
    | ':' => Ok({token: Colon, location})
    | '=' => Ok({token: Equals, location})
    | '@' => Ok({token: At, location})
    | '[' => Ok({token: BracketOpen, location})
    | ']' => Ok({token: BracketClose, location})
    | '{' => Ok({token: BraceOpen, location})
    | '|' => Ok({token: Pipe, location})
    | '}' => Ok({token: BraceClose, location})
    | '.' =>
      if (isChar(source, position + 1, '.') && isChar(source, position + 2, '.')) {
        Ok({
          token: Spread,
          location: {
            ...location,
            end_: position + 3,
          },
        });
      } else if (position + 1 >= String.length(source)) {
        syntaxError("Unexpected End of File");
      } else {
        syntaxError("Unexpected Character" ++ String.make(1, source.[position + 1]));
      }
    | 'A'..'Z'
    | 'a'..'z'
    | '_' => Ok(readName(source, ~start=position, ~line, ~column))
    | '0'..'9'
    | '-' => readNumber(source, ~start=position, ~line, ~column)
    | '"' => readString(source, ~start=position, ~line, ~column)
    | '#' => Ok(readComment(source, ~start=position, ~line, ~column))
    | char => syntaxError("Unexpected Character" ++ String.make(1, char))
    };
  };
};

let make = source => {
  source,
  curr: {
    token: StartOfFile,
    location: {
      start: 0,
      end_: 0,
      column: 0,
      line: 1,
    },
  },
  line: 1,
  lineStart: 0,
};

let lookahead =
  fun
  | {curr: {token: EndOfFile} as tokenResult} => Result.Ok(tokenResult)
  | lexer => {
      let rec skipComment = tokenResult => {
        let%Result tokenResult' = readToken(lexer, tokenResult);
        switch (tokenResult') {
        | {token: Comment(_)} => skipComment(tokenResult')
        | token => Ok(token)
        };
      };

      skipComment(lexer.curr);
    };

let advance = lexer => {
  let%Result curr = lookahead(lexer);
  lexer.curr = curr;
  Ok(curr);
};

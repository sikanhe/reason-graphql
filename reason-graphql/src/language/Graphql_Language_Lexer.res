module Result = {
  include Belt.Result
  let let_ = flatMap
}

exception SyntaxError(string)
let syntaxError = a => raise(SyntaxError(a))

type location = {
  start: int,
  end_: int,
  line: int,
  column: int,
}

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
  | Comment(string)

let tokenKind = x =>
  switch x {
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
  | Comment(_) => "Comment"
  }

type tokenResult = {
  token: token,
  location: location,
}

type t = {
  source: string,
  mutable curr: tokenResult,
  mutable line: int,
  mutable lineStart: int,
}

/* (line:col) kind: <kind>, value: <value> */
let tokenDesc = ({token, location}) =>
  "(" ++
  (string_of_int(location.line) ++
  (":" ++
  (string_of_int(location.column) ++
  (")" ++
  switch token {
  | Name(v) => " kind: '" ++ (tokenKind(token) ++ ("', value: " ++ v))
  | Int(v) => " kind: '" ++ (tokenKind(token) ++ ("', value: " ++ v))
  | Float(v) => " kind: '" ++ (tokenKind(token) ++ ("', value: " ++ v))
  | String(v) => " kind: '" ++ (tokenKind(token) ++ ("', value: " ++ v))
  | Comment(v) => " kind: '" ++ (tokenKind(token) ++ ("', value: " ++ v))
  | token => " kind: '" ++ (tokenKind(token) ++ "'")
  }))))

let isChar = (source, position, char) =>
  position < String.length(source) && String.get(source, position) == char

@ocaml.doc("
 * Reads from source starting at startPosition until it finds a non-whitespace
 * character, then returns the position of that character for lexing.
 ")
let positionAfterWhitespace = (lexer, startPosition) => {
  let {source} = lexer

  let rec aux = position =>
    if position >= String.length(source) {
      position
    } else {
      switch String.get(source, position) {
      | bom if int_of_char(bom) == 0xFEFF => position + 1
      | ' '
      | ','
      | '\t' =>
        aux(position + 1)
      | '\n' =>
        let newPosition = position + 1
        lexer.line = lexer.line + 1
        lexer.lineStart = newPosition
        aux(newPosition)
      | '\r' =>
        let newPosition = isChar(source, position + 1, '\n') ? position + 2 : position + 1

        lexer.line = lexer.line + 1
        lexer.lineStart = newPosition
        aux(newPosition)
      | _ => position
      }
    }

  aux(startPosition)
}

let isNameChar = x =>
  switch x {
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '0' .. '9'
  | '_' => true
  | _ => false
  }

@ocaml.doc("
 * Reads an alphanumeric + underscore name from the source.
 *
 * [_A-Za-z][_0-9A-Za-z]*
 ")
let readName = (source, ~start, ~line, ~column): tokenResult => {
  let rec aux = position =>
    switch String.get(source, position) {
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '_' =>
      aux(position + 1)
    | _ => {
        token: Name(String.sub(source, start, position - start)),
        location: {
          start: start,
          line: line,
          end_: position,
          column: column,
        },
      }
    }

  aux(start)
}

@ocaml.doc("
 * #[\u0009\u0020-\uFFFF]*
 ")
let readComment = (source, ~start, ~line, ~column): tokenResult => {
  let rec aux = position =>
    if position > String.length(source) {
      position
    } else {
      switch String.get(source, position) {
      | '\r'
      | '\n' => position
      | _ => aux(position + 1)
      }
    }

  let position = aux(start)

  {
    token: Comment(String.sub(source, start, position - start)),
    location: {
      start: start,
      line: line,
      end_: position,
      column: column,
    },
  }
}

let readDigits = (source, startingPosition): int => {
  let rec aux = (source, pos) =>
    if pos >= String.length(source) {
      pos
    } else {
      switch String.get(source, pos) {
      | '0' .. '9' => aux(source, pos + 1)
      | c if pos === startingPosition =>
        syntaxError("Invalid number, expected digit but got: " ++ String.make(1, c))
      | _ => pos
      }
    }

  aux(source, startingPosition)
}

@ocaml.doc("
 * Reads a number token from the source file, either a float
 * or an int depending on whether a decimal point appears.
 *
 * Int:   -?(0|[1-9][0-9]*)
 * Float: -?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(+|-)?[0-9]+)?
 ")
let readNumber = (source, ~start, ~line, ~column): tokenResult => {
  let isFloat = ref(false)
  let position = ref(start)

  if String.get(source, start) == '-' {
    position := position.contents + 1
  }

  let () = if String.get(source, position.contents) == '0' {
    position := position.contents + 1
    switch String.get(source, position.contents) {
    | '0' .. '9' as char =>
      syntaxError("Invalid number, unexpected digit after 0: " ++ String.make(1, char))
    | _ => ()
    }
  } else {
    let pos = readDigits(source, position.contents)
    position := pos
  }

  let () = if isChar(source, position.contents, '.') {
    isFloat := true
    position := position.contents + 1

    let pos = readDigits(source, position.contents)
    position := pos
  } else {
    ()
  }

  let () = if isChar(source, position.contents, 'E') || isChar(source, position.contents, 'e') {
    isFloat := true
    position := position.contents + 1

    if isChar(source, position.contents, '+') || isChar(source, position.contents, '-') {
      position := position.contents + 1
    }

    let pos = readDigits(source, position.contents)
    position := pos
  } else {
    ()
  }

  let loc = {start: start, end_: position.contents, line: line, column: column}

  let tok = \"!"(isFloat)
    ? Float(String.sub(source, start, position.contents - start))
    : Int(String.sub(source, start, position.contents - start))

  {token: tok, location: loc}
}

@ocaml.doc("
 * Converts a hex character to its integer value.
 * '0' becomes 0, '9' becomes 9
 * 'A' becomes 10, 'F' becomes 15
 * 'a' becomes 10, 'f' becomes 15
 *
 * Returns -1 on error.
 ")
let char2hex = c =>
  c >= 48 && c <= 57
    ? c - 48 // 0-9
    : switch c >= 65 && c <= 70 {
      | true => c - 55 // c-F
      | false => c >= 97 && c <= 102 ? c - 87 : -1 // a-f
      }

@ocaml.doc("
 * Converts four hexadecimal chars to the integer that the
 * string represents. For example, uniCharCode('0','0','0','f')
 * will return 15, and uniCharCode('0','0','f','f') returns 255.
 *
 * Returns a negative number on error, if a char was invalid.
 *
 * This is implemented by noting that char2hex() returns -1 on error,
 * which means the result of ORing the char2hex() will also be negative.
 ")
let uniCharCode = (a, b, c, d) =>
  lor(lor(lor(lsl(char2hex(a), 12), lsl(char2hex(b), 8)), lsl(char2hex(c), 4)), char2hex(d))

@ocaml.doc("
 * Reads a string token from the source file.
 *
 * \"([^\"\\\u000A\u000D]|(\\(u[0-9a-fA-F]{4}|[\"\\/bfnrt])))*\"
 ")
let readString = (source, ~start, ~line, ~column): tokenResult => {
  let rec aux = (value, position, chunkStart) => {
    if position >= String.length(source) {
      syntaxError("Unterminated string")
    }

    switch String.get(source, position) {
    | '\n' => syntaxError("Unterminated string")
    | '"' => {
        token: String(value ++ String.sub(source, chunkStart, position - chunkStart)),
        location: {
          line: line,
          column: column,
          start: start,
          end_: position + 1,
        },
      }
    | c if Char.code(c) == 92 =>
      let newPosition = ref(position + 1)
      let code = String.get(source, newPosition.contents)->Char.code

      let rest = switch code {
      | 34 => "\""
      | 47 => "/"
      | 92 => "\\"
      | 98 => "\b"
      | 102 => "\\f"
      | 110 => "\n"
      | 114 => "\r"
      | 116 => "\t"
      // u
      | 117 =>
        let charCode = uniCharCode(
          String.get(source, position + 1)->Char.code,
          String.get(source, position + 2)->Char.code,
          String.get(source, position + 3)->Char.code,
          String.get(source, position + 4)->Char.code,
        )

        if charCode < 0 {
          syntaxError(
            "Invalid character escape sequence: " ++
            ("\\u" ++
            (String.sub(source, position + 1, position + 5) ++ ".")),
          )
        } else {
          newPosition := newPosition.contents + 4
          Char.chr(charCode) |> String.make(1)
        }

      | _ =>
        syntaxError("Invalid character escape sequence: \\" ++ (Char.chr(code) |> String.make(1)))
      }
      let value =
        value ++ (String.sub(source, chunkStart, newPosition.contents - chunkStart - 1) ++ rest)

      let nextPosition = newPosition.contents + 1
      aux(value, nextPosition, nextPosition)

    | _ => aux(value, position + 1, chunkStart)
    }
  }

  aux("", start + 1, start + 1)
}

@ocaml.doc("
 * Gets the next token from the source starting at the given position.
 *
 * This skips over whitespace until it finds the next lexable token, then lexes
 * punctuators immediately or calls the appropriate helper function for more
 * complicated tokens.
 ")
let readToken = (lexer, from): tokenResult => {
  let {source} = lexer
  let position = positionAfterWhitespace(lexer, from)
  let line = lexer.line
  let column = 1 + position - lexer.lineStart
  let sourceLength = String.length(source)
  let singleWidth = {start: position, end_: position + 1, line: line, column: column}

  if position >= String.length(source) {
    {
      token: EndOfFile,
      location: {
        start: sourceLength,
        end_: sourceLength,
        line: line,
        column: column,
      },
    }
  } else {
    switch String.get(source, position) {
    | '!' => {token: Bang, location: singleWidth}
    | '$' => {token: Dollar, location: singleWidth}
    | '&' => {token: Amp, location: singleWidth}
    | '(' => {token: ParenOpen, location: singleWidth}
    | ')' => {token: ParenClose, location: singleWidth}
    | ':' => {token: Colon, location: singleWidth}
    | '=' => {token: Equals, location: singleWidth}
    | '@' => {token: At, location: singleWidth}
    | '[' => {token: BracketOpen, location: singleWidth}
    | ']' => {token: BracketClose, location: singleWidth}
    | '{' => {token: BraceOpen, location: singleWidth}
    | '|' => {token: Pipe, location: singleWidth}
    | '}' => {token: BraceClose, location: singleWidth}
    | '.' =>
      if isChar(source, position + 1, '.') && isChar(source, position + 2, '.') {
        {
          token: Spread,
          location: {
            start: position,
            end_: position + 3,
            line: line,
            column: column,
          },
        }
      } else if position + 1 >= String.length(source) {
        syntaxError("Unexpected End of File")
      } else {
        syntaxError("Unexpected Character" ++ String.make(1, String.get(source, position + 1)))
      }
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '_' =>
      readName(source, ~start=position, ~line, ~column)
    | '0' .. '9'
    | '-' =>
      readNumber(source, ~start=position, ~line, ~column)
    | '"' => readString(source, ~start=position, ~line, ~column)
    | '#' => readComment(source, ~start=position, ~line, ~column)
    | char => syntaxError("Unexpected Character" ++ String.make(1, char))
    }
  }
}

let make = source => {
  source: source,
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
}

let lookahead = x =>
  switch x {
  | {curr: {token: EndOfFile} as tokenResult} => tokenResult
  | lexer =>
    let rec skipComment = prevToken => {
      let token = readToken(lexer, prevToken.location.end_)
      switch token {
      | {token: Comment(_)} => skipComment(token)
      | token => token
      }
    }

    skipComment(lexer.curr)
  }

let advance = lexer => {
  let curr = lookahead(lexer)
  lexer.curr = curr
  curr
}

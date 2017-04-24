open Shim
open Parsing_framework

open Syntax.Abstract
open Syntax.Abstract.Atom
open Syntax.Abstract.Atom.Option

module Atom = struct
  let word =
    Char.one_or_more Letters.Case.lower

  let command =
    word >>= fun name ->
    return (Command name)

  let argument =
    char '<' >=>
    word     >>= fun name ->
    char '>' >=>
    return (Argument name)

  let long_option =
    string "--" >=>
    word     >>= fun keyword ->
    option (
      string "=<" >=>
      word        >>= fun parameter ->
      char '>'    >=>
      return parameter
    ) >>= fun parameter ->
    return (Option (Long (keyword, parameter)))

  let short_options =
    char '-' >=>
    word     >>= fun letters ->
    return (Option (Short letters))

  let parser = command <|> argument <|> long_option <|> short_options
end

let parenthesised parser =
  char '(' >=>
  parser   >>= fun result ->
  char ')' >=>
  return result

let (&) = (|>)

let whitespace =
  zero_or_more Char.whitespace

let token literal =
  string literal >=>
  whitespace >=>
  return literal

module Token = struct
  let ellipsis = token "..."

  let pipe = token "|"

  module Parenthesis = struct
    let left = token "("
    let right = token ")"
  end

  module Bracket = struct
    let left = token "["
    let right = token "]"
  end
end

let bracketed parser =
  Token.Bracket.left  >=>
  parser              >>= fun result ->
  Token.Bracket.right >=>
  return result

let rec one_or_more source = source &
  group          >>= fun child ->
  Token.ellipsis >=>
  return (One_or_more child)

and sequence_list source = source &
  separated ~by:whitespace not_sequence

and sequence source = source &
  sequence_list >>= function
  | [single] -> return single
  | many -> return (Sequence many)

and alternative source = source &
  separated ~by:Token.pipe sequence >>= function
  | [single] -> return single
  | many -> return (Alternative many)

and optional source = source &
  bracketed sequence_list >>= fun items ->
  return (Optional items)

and atom source = source &
  Atom.parser >>= fun atom ->
  whitespace >=>
  return (Atom atom)

and group source = source &
  atom <|> parenthesised sequence

and not_sequence source = source &
  one_or_more </> atom

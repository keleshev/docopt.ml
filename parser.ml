open Shim
open Parsing_framework

open Syntax.Abstract
open Syntax.Abstract.Atom
open Syntax.Abstract.Atom.Option

let fix f_nonrec x =
  let rec f = lazy (fun x -> f_nonrec (Lazy.force f) x) in
  (Lazy.force f) x

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

let wrapped left parser right =
  left >=>
  parser >>= fun result ->
  right >=>
  return result

let in_brackets parser =
  wrapped Token.Bracket.left parser Token.Bracket.right

let in_parenthesis parser =
  wrapped Token.Parenthesis.left parser Token.Parenthesis.right

let atom =
  Atom.parser >>= fun a ->
  whitespace >=>
  return (Atom a)


let expr = fix @@ fun expr ->

  let optional =
    in_brackets expr >>= fun item ->
    return (Optional [item])
  in

  let root = atom <|> optional <|> in_parenthesis expr in

  let one_or_more =
    root >>= fun child ->
    default child (Token.ellipsis >=> return (One_or_more child))
  in
  let sequence =
    separated ~by:whitespace one_or_more >>= function
(*  Also valid: Parsing_framework.one_or_more one_or_more >>= function *)
    | [single] -> return single
    | multiple -> return (Sequence multiple)
  in
  let alternative =
    separated ~by:Token.pipe sequence >>= function
    | [single] -> return single
    | multiple -> return (Alternative multiple)
  in
  alternative

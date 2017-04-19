module Parsing = Parsing

module Atom = struct
  type t =
    | Command of string
    | Argument of string
    | Long_option of string * string option
    | Short_options of string

  module Parser = struct
    open Parsing

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
      char '-' >=> (* string "--" *)
      char '-' >=>
      word     >>= fun keyword ->
      option (
        char '=' >=>
        char '<' >=>
        word     >>= fun parameter ->
        char '>' >=>
        return parameter
      ) >>= fun parameter ->
      return (Long_option (keyword, parameter))

    let short_options =
      char '-' >=>
      word     >>= fun letters ->
      return (Short_options letters)

    let atom = command <|> argument <|> long_option <|> short_options
  end

  let parser = Parser.atom
end

type t =
  | Atom of Atom.t
  | One_or_more of t
  | Optional of t list
  | Sequence of t list
  | Alternative of t * t

module Parser = struct
  open Parsing

  let parenthesised parser =
    char '(' >=>
    parser   >>= fun result ->
    char ')' >=>
    return result

  let (&) = (|>)

  let whitespace =
    one_or_more (Char.set " \t\n\r")

  let rec one_or_more source = source &
    group       >>= fun child ->
    char '.'    >=>
    char '.'    >=>
    char '.'    >=>
    return (One_or_more child)

  and sequence source = source &
    separated ~by:whitespace not_sequence >>= function
    | [] -> failwith "`separated` must never return []"
    | [singleton] -> return singleton
    | many -> return (Sequence many)

  and atom source = source &
    Atom.parser >>= fun atom ->
    return (Atom atom)

  and group source = source &
    atom <|> parenthesised sequence

  and not_sequence source = source &
    one_or_more <|> atom

end



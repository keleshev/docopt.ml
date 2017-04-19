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
      string "--" >=>
      word     >>= fun keyword ->
      option (
        string "=<" >=>
        word        >>= fun parameter ->
        char '>'    >=>
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
  | Alternative of t list

module Parser = struct
  open Parsing

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

  let rec one_or_more source = source &
    group          >>= fun child ->
    Token.ellipsis >=>
    return (One_or_more child)

  and sequence source = source &
    separated ~by:whitespace not_sequence >>= function
    | [] -> failwith "`separated` must never return []"
    | [single] -> return single
    | many -> return (Sequence many)

  and alternative source = source &
    separated ~by:Token.pipe sequence >>= function
    | [] -> failwith "`separated` must never return []"
    | [single] -> return single
    | many -> return (Alternative many)

  and atom source = source &
    Atom.parser >>= fun atom ->
    whitespace >=>
    return (Atom atom)

  and group source = source &
    atom <|> parenthesised sequence

  and not_sequence source = source &
    one_or_more <|> atom

end



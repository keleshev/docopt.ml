open Shim

module Parsing = Parsing

module Atom = struct
  module Option = struct
    type t =
      | Long of string * string option
      | Short of string
  end

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
    one_or_more <|> atom

end

module Argv = struct
  module Token = struct
    type t =
      | Long_option of string * string option
      | Short_options of string
      | Argument of string
      | Dash
  end

  open Token

  let rec tokenize = function
    | [] -> []
    | "-" :: tail -> Dash :: tokenize tail
    | "--" :: tail -> List.map tail ~f:(fun s -> Argument s)
    | head :: tail when String.is_prefix ~prefix:"--" head ->
        let name, argument = String.partition ~on:'=' head in
        Long_option (name, argument) :: tokenize tail
    | head :: tail when String.is_prefix ~prefix:"-" head ->
        Short_options head :: tokenize tail
    | head :: tail ->
        Argument head :: tokenize tail
end

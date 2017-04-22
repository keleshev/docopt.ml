let (=>) left right = print_string (if left = right then "." else "F")

open Docopt

module SyntaxTest = struct
  open Syntax

  let parse parser source_string =
    let open Parsing in
    let parse_all =
      parser >>= fun result ->
      eof >=>
      return result
    in
    parse parse_all (Source.of_string source_string)

  module AtomTest = struct
    open Atom
    open Atom.Option

    module ParserTest = struct
      open Parser

      let () = ()
        ; parse atom "hai"
            => Some (Command "hai")

        ; parse atom "<hai>"
            => Some (Argument "hai")

        ; parse atom "--hai"
            => Some (Option (Long ("hai", None)))

        ; parse atom "--hai=<bye>"
            => Some (Option (Long ("hai", Some "bye")))

        ; parse atom "-hai"
            => Some (Option (Short "hai"))
    end
  end

  module ParserTest = struct
    open Parser

    let hai = Atom (Atom.Command "hai")
    let bye = Atom (Atom.Command "bye")

    let () = ()
      ; parse one_or_more "hai..."
          => Some (One_or_more hai)

      ; parse one_or_more "hai......"
          => None

      ; parse one_or_more "(hai...)..."
          => Some (One_or_more (One_or_more hai))

      ; parse one_or_more "(hai... )..."
          => Some (One_or_more (One_or_more hai))

      ; parse sequence "hai bye"
          => Some (Sequence [hai; bye])

      ; parse sequence "hai \n\r\t bye"
          => Some (Sequence [hai; bye])

      ; parse sequence "hai... bye"
          => Some (Sequence [One_or_more hai; bye])

      ; parse alternative "hai|bye"
          => Some (Alternative [hai; bye])

      ; parse alternative "hai| bye"
          => Some (Alternative [hai; bye])

      ; parse alternative "hai | bye"
          => Some (Alternative [hai; bye])

      ; parse optional "[hai]"
          => Some (Optional [hai])

  (*  ; parse optional "[[hai]]"
          => Some (Optional [Optional [hai]])*)

      ; parse optional "[hai bye]"
          => Some (Optional [hai; bye])
  end
end

module ArgvTest = struct
  open Argv
  open Argv.Token

  let () = ()
    ; tokenize [] => []
    ; tokenize ["hai"] => [Argument "hai"]
    ; tokenize ["hai"; "bye"] => [Argument "hai"; Argument "bye"]

    ; tokenize ["--hai"] => [Long_option ("--hai", None)]
    ; tokenize ["--hai=bye"] => [Long_option ("--hai", Some "bye")]
    ; tokenize ["--hai="] => [Long_option ("--hai", Some "")]

    ; tokenize ["-h"] => [Short_options "-h"]
    ; tokenize ["-hai"] => [Short_options "-hai"]

    ; tokenize ["--"; "--hai"] => [Argument "--hai"]
    ; tokenize ["--"; "--"] => [Argument "--"]

    ; tokenize ["-"] => [Dash]
    ; tokenize ["--"; "-"] => [Argument "-"]
end

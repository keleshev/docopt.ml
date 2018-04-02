let (=>) left right = print_string (if left = right then "." else "F")

open Syntax.Abstract
open Syntax.Abstract.Atom
open Syntax.Abstract.Atom.Option

let parse parser source_string =
  let open Parsing_framework in
  let parse_all =
    parser >>= fun result ->
    end_of_input >=>
    return result
  in
  parse parse_all (Source.of_string source_string)

module AtomParserTest = struct
  let atom = Parser.Atom.parser

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

let hai = Atom (Command "hai")
let bye = Atom (Command "bye")

let () = let open Parser in ()
  ; parse expr "hai "
      => Some (Atom (Command "hai"))

  ; parse expr "hai..."
      => Some (One_or_more hai)

  ; parse expr "(hai)"
      => Some (hai)

  ; parse expr "(hai...)"
      => Some (One_or_more hai)

  ; parse expr "hai......"
      => None

  ; parse expr "(hai...)..."
      => Some (One_or_more (One_or_more hai))

  ; parse expr "(hai... )..."
      => Some (One_or_more (One_or_more hai))

  ; parse expr "hai bye"
      => Some (Sequence [hai; bye])

  ; parse expr "hai \n\r\t bye"
      => Some (Sequence [hai; bye])

  ; parse expr "hai... bye"
      => Some (Sequence [One_or_more hai; bye])

  ; parse expr "hai|bye"
      => Some (Alternative [hai; bye])

  ; parse expr "hai| bye"
      => Some (Alternative [hai; bye])

  ; parse expr "hai | bye"
      => Some (Alternative [hai; bye])

  ; parse expr "hai | bye hai"
      => Some (Alternative [hai; Sequence [bye; hai]])

  ; parse expr "[hai]"
      => Some (Optional [hai])

  ; parse expr "[[hai]]"
      => Some (Optional [Optional [hai]])

  ; parse expr "[hai bye]"
      => Some (Optional [Sequence [hai; bye]])



let (=>) left right = print_string (if left = right then "." else "F")

let parse parser source_string =
  let open Parsing in
  let parse_all =
    parser >>= fun result ->
    eof >=>
    return result
  in
  parse parse_all (Source.of_string source_string)

open Docopt

module AtomTest = struct
  open Atom

  module ParserTest = struct
    open Parser

    let () = ()
      ; parse atom "hai"
          => Some (Command "hai")

      ; parse atom "<hai>"
          => Some (Argument "hai")

      ; parse atom "--hai"
          => Some (Long_option ("hai", None))

      ; parse atom "--hai=<bye>"
          => Some (Long_option ("hai", Some "bye"))

      ; parse atom "-hai"
          => Some (Short_options "hai")
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
end

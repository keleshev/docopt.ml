let (=>) left right = print_string (if left = right then "." else "F")

let parse parser string =
  let open Parsing in
  let parse_all =
    parser >>= fun result ->
    eof >=>
    return result
  in
  parse parse_all (Source.of_string string)

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

  let () = ()
    ; parse (one_or_more ()) "hai..."
        => Some (One_or_more (Atom (Atom.Command "hai")))
    (* weird...... *)
    ; parse (one_or_more ()) "hai......"
        => Some (One_or_more (One_or_more (Atom (Atom.Command "hai"))))
end

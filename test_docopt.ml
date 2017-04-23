let (=>) left right = print_string (if left = right then "." else "F")

open Docopt

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

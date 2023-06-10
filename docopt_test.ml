let (=>) left right = print_char (if left = right then '.' else 'F')

open Docopt.Pattern
let c = Discrete (Command "c")
let a = Discrete (Argument "<a>")
let b = Discrete (Argument "<b>")
let (<*>) l r = Sequence (l, r)

module Test_integration = struct
  let doc = c <*> Multiple a <*> b (* c <a>... <b> *)

  let main =
    let open Docopt in
    let+ () = get unit "c"
    and+ a = get (list string) "<a>"
    and+ b = get string "<b>" in
    a, b

  let () =
    Docopt.run main ~doc ~argv:["c"; "a"; "b"; "c"]
      => Ok (["a"; "b"], "c");
end

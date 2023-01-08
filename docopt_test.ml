let (=>) left right = print_char (if left = right then '.' else 'F')





let _eval_string_term = 
  let term = Docopt.(term Type.String Term.(Argument "<x>")) in
  Docopt.eval term ~argv:["42"] => Ok "42"

let _eval_int_term = 
  let term = Docopt.(term Type.Int Term.(Argument "<x>")) in
  Docopt.eval term ~argv:["42"] => Ok 42

let _map_string_to_int =
  let term = 
    Docopt.(term Type.String Term.(Argument "<x>")) 
    |> Docopt.map int_of_string
  in
  Docopt.eval term ~argv:["42"] => Ok 42 

(*let _eval_sequence_as_pair =
  let term = Docopt.(term Type.(Pair (String, Int)) Term.(Sequence (Argument "<x>", Argument "<y>"))) in
  Docopt.eval term ~argv:["1"; "2"] => Ok ("1", 2)*)


(*
let (let+), (and+) = Docopt.(map, product)
let term = 
  Docopt.(Term Term.(Argument "<x>"))
  |> Docopt.Map (fun x -> Int.of_string x)

let () =
  Docopt.eval term ~argv:["42"] => 42
*)

(*
let term =
  let+ () = Docopt.(term Type.Unit (Command "cp"))
  and+ source = Docopt.(term Type.String (Argument "<source>"))
  and+ dest = Docopt.(term Type.String (Argument "<dest>")) in
  `cp (source, dest)
  
let () =
  let result = Docopt.eval term ~argv:["cp"; "hello"; "world"] in
  result => Ok (`cp ("hello", "world"))
 *)



let () = 
  Docopt.hello => "world";

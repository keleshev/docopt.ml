let (=>) left right = print_char (if left = right then '.' else 'F')


let (let+) term callback = Docopt.map callback term
let (and+) = Docopt.both

let env_to_list m = Docopt.Env.to_seq m |> List.of_seq



let _eval_string_term = 
  let term = Docopt.(term Type.String "<x>") in
  Docopt.eval term ~argv:["42"] => Ok ("42", [])

let _eval_int_term = 
  let term = Docopt.(term Type.Int "<x>") in
  Docopt.eval term ~argv:["42"] => Ok (42, [])

let _map_string_to_int =
  let term = 
    Docopt.(term Type.String "<x>") 
    |> Docopt.map int_of_string
  in
  Docopt.eval term ~argv:["42"] => Ok (42, [])

let _map_string_to_int_using_let_syntax =
  let term = 
    let+ x = Docopt.(term Type.String "<x>") in
    `Return (int_of_string x)
  in
  Docopt.eval term ~argv:["42"] => Ok (`Return 42, [])

(*let _test =
  let doc = Docopt.Doc.(Atom (Argument "<x>")) in
  let term =
    let+ x = Docopt.(term Type.Int Doc.(Argument "<x>")) in
    `Return x
  in
  Docopt.eval' term ~doc ~argv:["42"] => Ok (`Return 42)*)

  
module Test_infer = struct
  open Docopt.Pattern
  open Docopt.Parameter
  let _infer_type =
    let pattern = Sequence (Parameter (Argument "<x>"), Parameter (Command "a")) in
    Docopt.infer pattern |> env_to_list => Docopt.Env.[
      Argument "<x>", One;
      Command "a", One;
    ]
  
  let _infer_repeating_sequence =
    let pattern = Sequence (Parameter (Argument "<x>"), Parameter (Argument "<x>")) in
    Docopt.infer pattern |> env_to_list => Docopt.Env.[
      Argument "<x>", List;
    ]
  
  let _infer_optional_sequence =
    let pattern = Docopt.Pattern.(Optional (Parameter (Argument "<x>"))) in
    Docopt.infer pattern |> env_to_list => Docopt.Env.[
      Argument "<x>", Option;
    ]
  
  let _infer_repeating_argument =
    let pattern = One_or_more (Parameter (Argument "<x>")) in
    Docopt.infer pattern |> env_to_list => Docopt.Env.[
      Argument "<x>", List;
    ]
  
  let _infer_alternating_argument_command =
    let pattern = Either (Parameter (Argument "<x>"), Parameter (Command "c")) in
    Docopt.infer pattern |> env_to_list => Docopt.Env.[
      Argument "<x>", Option;
      Command "c", Option;
    ]
  
  let _infer_alternating_same_argument =
    let pattern = Docopt.Pattern.(Either (Parameter (Argument "<x>"), Parameter (Argument "<x>"))) in
    Docopt.infer pattern |> env_to_list => Docopt.Env.[
      Argument "<x>", One;
    ]
  
  let _infer_alternating_same_argument_different_collection =
    let pattern = Either (Parameter (Argument "<x>"), Optional (Parameter (Argument "<x>"))) in
    Docopt.infer pattern |> env_to_list => Docopt.Env.[
      Argument "<x>", Option;
    ]
  
  let _infer_alternating_same_command_different_collection =
    let pattern = Docopt.Pattern.(Either (Parameter (Command "c"), One_or_more (Parameter (Command "c")))) in
    Docopt.infer pattern |> env_to_list => Docopt.Env.[
      Command "c", List;
    ]
end
(*let _infer_type =
  let doc = "Usage: prog a [b] c... <x> [<y>] <z>..." in
  let argv = ["set"; "1"; "2"] in
  Docopt.eval ~doc ~argv => Ok [
    "a", Unit;
    "b", Bool;
    "a", Int;
    "<x>", String;
    "<y>", Option String;
    "<z>", List1 String;
  ]*)

(*let _eval_sequence_as_pair =
  let syntax = Docopt.Doc.(Sequence (Argument "<x>", Argument "<y>")) in
  let term = Docopt.(term Type.(Pair (String, Int)) syntax) in
  Docopt.eval term ~argv:["1"; "2"] => Ok (("1", 2), [])*)

let _eval_sequence_as_two_bindings =
  let term =
    let+ first = Docopt.(term Type.String "<x>")
    and+ second = Docopt.(term Type.Int "<y>") in
    `Return (first, second)
  in
  Docopt.eval term ~argv:["1"; "2"] => Ok (`Return ("1", 2), [])

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

let (=>) left right = print_char (if left = right then '.' else 'F')


let env_to_list m = Docopt.Atom.Map.to_seq m |> List.of_seq


module Occurence = Docopt.Occurence
  
module Test_infer = struct
  open Docopt.Pattern
  open Docopt.Atom
  let _infer_type =
    let pattern = Sequence (Discrete (Argument "<x>"), Discrete (Command "a")) in
    Docopt.Occurence.infer pattern |> env_to_list => [
      Argument "<x>", Required;
      Command "a", Required;
    ]
  
  let _infer_repeating_sequence =
    let pattern = Sequence (Discrete (Argument "<x>"), Discrete (Argument "<x>")) in
    Docopt.Occurence.infer pattern |> env_to_list => [
      Argument "<x>", Occurence.Multiple;
    ]
  
  let _infer_optional_sequence =
    let pattern = Docopt.Pattern.(Optional (Discrete (Argument "<x>"))) in
    Docopt.Occurence.infer pattern |> env_to_list => [
      Argument "<x>", Occurence.Multiple;
    ]
  
  let _infer_repeating_argument =
    let pattern = Multiple (Discrete (Argument "<x>")) in
    Docopt.Occurence.infer pattern |> env_to_list => [
      Argument "<x>", Occurence.Multiple;
    ]
  
  let _infer_alternating_argument_command =
    let pattern = Junction (Discrete (Argument "<x>"), Discrete (Command "c")) in
    Docopt.Occurence.infer pattern |> env_to_list => [
      Argument "<x>", Occurence.Optional;
      Command "c", Occurence.Optional;
    ]
  
  let _infer_alternating_same_argument =
    let pattern = Docopt.Pattern.(Junction (Discrete (Argument "<x>"), Discrete (Argument "<x>"))) in
    Docopt.Occurence.infer pattern |> env_to_list => [
      Argument "<x>", Required;
    ]
  
  let _infer_alternating_same_argument_different_collection =
    let pattern = Junction (Discrete (Argument "<x>"), Optional (Discrete (Argument "<x>"))) in
    Docopt.Occurence.infer pattern |> env_to_list => [
      Argument "<x>", Occurence.Optional;
    ]
  
  let _infer_alternating_same_command_different_collection =
    let pattern = Docopt.Pattern.(Junction (Discrete (Command "c"), Multiple (Discrete (Command "c")))) in
    Docopt.Occurence.infer pattern |> env_to_list => [
      Command "c", Occurence.Multiple;
    ]
end


let () = 
  Docopt.hello => "world";

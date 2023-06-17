let (=>) left right = print_char (if left = right then '.' else 'F')

open Docopt.Pattern
let (<*>) l r = Sequence (l, r)
let (!) source = Discrete (Docopt.Atom.parse source)

module Test_unit_bool_int = struct
  let _doc = "usage: prog unit [bool] int..."
  let doc = !"unit" <*> Optional !"bool" <*> Multiple !"int"

  let main =
    let open Docopt in
    let+ () = get unit "unit"
    and+ bool = get bool "bool"
    and+ int = get int "int" in
    bool, int

  let () =
    Docopt.run main ~doc ~argv:["unit"; "bool"; "int"; "int"]
      => Ok (true, 2) 

  let () =
    Docopt.run main ~doc ~argv:["unit"; "int"]
      => Ok (false, 1)
end

module Test_string_option_list = struct
  let _doc = "usage: prog <string> [<option>] <list>..."
  let doc = !"<string>" <*> Optional !"<option>" <*> Multiple !"<list>"

  let main =
    let open Docopt in
    let+ s = get string "<string>"
    and+ o = get (option string) "<option>"
    and+ l = get (list string) "<list>" in
    s, o, l

  let () =
    Docopt.run main ~doc ~argv:["a"; "b"; "c"; "d"]
      => Ok ("a", Some "b", ["c"; "d"]) 

  let () =
    Docopt.run main ~doc ~argv:["a"; "b"]
      => Ok ("a", None, ["b"]) 
end

module Test_russ_cox_pathological_example = struct
  let _doc = "usage: prog [a] [a] [a] [a] a a a a"
  let doc = Optional !"a" <*> Optional !"a" 
        <*> Optional !"a" <*> Optional !"a" 
        <*> !"a" <*> !"a" <*> !"a" <*> !"a"

  let main = Docopt.(get int "a")

  let _ =
    Docopt.run main ~doc ~argv:["a";"a";"a"; "a"]
      => Ok 4
end

(*module Test_russ_cox_pathological_example' = struct
  let _doc = "usage: prog [a] [a] [a] a a a"
  let aq4 = Optional !"a" <*> Optional  !"a" <*>Optional  !"a" <*>Optional  !"a"
  let a4 = !"a" <*> !"a" <*> !"a" <*> !"a"
  let aq16 = aq4 <*> aq4 <*> aq4 <*> aq4
  let a16 = a4 <*> a4 <*> a4 <*> a4
  let doc = aq16 <*> aq16 <*> a16 <*> a16

  let main = Docopt.(get int "a")

  let _ =
    let a16 = ["a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"] in
    Docopt.run main ~doc ~argv:(a16 @ a16)
      => Ok 32
end*)

(* TODO: (a* )* *)


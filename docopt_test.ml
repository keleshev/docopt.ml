let (=>) left right = print_char (if left = right then '.' else 'F')

open Docopt.Pattern
module Doc = Docopt.Doc
module Map = Docopt.Map
let (<*>) l r = Sequence (l, r)
let (!) source = Discrete (Docopt.Atom.parse source)

module Test_unit_bool_int = struct
  let _doc = "usage: prog unit [bool] int..."
  let doc = Doc.{
    usage= !"unit" <*> Optional !"bool" <*> Multiple !"int";
    options=Map.empty;
  }

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
  let doc = Doc.{
    usage= !"<string>" <*> Optional !"<option>" <*> Multiple !"<list>";
    options=Map.empty;
  }

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

module Test_explicit_tuple = struct
  let _doc = "usage: prog <string> [<option>] <list>..."
  let doc = Doc.{
    usage= !"<string>" <*> Optional !"<option>" <*> Multiple !"<list>";
    options=Map.empty;
  }

  let main =
    let open Docopt in
    let+ s = get string "<string>"
    and+ o, l = tuple (get (option string) "<option>") (get (list string) "<list>") in
    s, o, l

  let () =
    Docopt.run main ~doc ~argv:["a"; "b"; "c"; "d"]
      => Ok ("a", Some "b", ["c"; "d"]) 
end

module Test_russ_cox_pathological_example = struct
  let _doc = "usage: prog [a] [a] [a] [a] a a a a"
  let doc = Doc.{
    usage= Optional !"a" <*> Optional !"a" 
        <*> Optional !"a" <*> Optional !"a" 
        <*> !"a" <*> !"a" <*> !"a" <*> !"a";
    options=Map.empty;
  }

  let main = Docopt.(get int "a")

  let () =
    Docopt.run main ~doc ~argv:["a";"a";"a"; "a"]
      => Ok 4
end

module Test_russ_cox_pathological_example_large = struct
  let _doc = "usage: prog [a]*32  a*32"
  let aq4 = Optional !"a" <*> Optional  !"a" <*>Optional  !"a" <*>Optional  !"a"
  let a4 = !"a" <*> !"a" <*> !"a" <*> !"a"
  let aq16 = aq4 <*> aq4 <*> aq4 <*> aq4
  let a16 = a4 <*> a4 <*> a4 <*> a4
  let doc = Doc.{
    usage=aq16 <*> aq16 <*> a16 <*> a16;
    options=Map.empty;
  }

  let main = Docopt.(get int "a")

  let () =
    let a8 = ["a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"] in
    Docopt.run main ~doc ~argv:(a8 @ a8 @ a8 @ a8)
      => Ok 32
end

module Test_another_pathological_example = struct
  let _doc = "usage: prog [[a]...]..."
  let doc = Doc.{
    usage=Multiple (Optional (Multiple (Optional !"a")));
    options=Map.empty;
  }

  let main = Docopt.(get int "a")

  let () =
    let a8 = ["a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"] in
    Docopt.run main ~doc ~argv:(a8 @ a8)
      => Ok 16
end

module Test_int_option = struct
  let _doc = "usage: prog --verbose..."
  let doc = Doc.{
    usage=Multiple !"--verbose";
    options=Map.of_list [
      "--verbose", Docopt.Option.{name="--verbose"; argument=false}; 
    ];
  }

  let main = Docopt.(get int "--verbose")

  let () = 
    Docopt.run main ~doc ~argv:["--verbose"; "--verbose"; "--verbose"]
      => Ok 3
end

module Test_int_option_2 = struct
  let _doc = "usage: prog [--verbose] [--verbose]"
  let doc = Doc.{
    usage= Optional (!"--verbose" <*> !"--verbose");
    options=Map.of_list [
      "--verbose", Docopt.Option.{name="--verbose"; argument=false}; 
    ];
  }

  let main = Docopt.(get int "--verbose")

  let () = 
    Docopt.run main ~doc ~argv:["--verbose"; "--verbose"] (*; "--verbose"]*)
      => Ok 2
end
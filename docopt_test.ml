let (=>) left right = print_char (if left = right then '.' else 'F')

open Docopt.Pattern
module Doc = Docopt.Doc
module Map = Docopt.Map
let (<*>) l r = Sequence (l, r)
let (!) source = Discrete (Docopt.Atom.of_string_unchecked source)
let option ?(argument=false) ?(synonyms=[]) canonical =
   Docopt.Option.{canonical; argument; synonyms}

module Test_unit_bool_int = struct
  let _doc = "usage: prog unit [bool] int..."
  let doc = Doc.{
    usage= !"unit" <*> Optional !"bool" <*> Multiple !"int";
    options=[];
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
    options=[];
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
    options=[];
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
    options=[];
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
    options=[];
  }

  let main = Docopt.(get int "a")

  let () =
    let a8 = ["a"; "a"; "a"; "a"; "a"; "a"; "a"; "a"] in
    Docopt.run main ~doc ~argv:(a8 @ a8 @ a8 @ a8)
      => Ok 32
end

module Test_another_russ_cox_pathological_example_potential_infinite_loop = struct
  let _doc = "usage: prog [[a]...]..."
  let doc = Doc.{
    usage=Multiple (Optional (Multiple (Optional !"a")));
    options=[];
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
    options=[option "--verbose"];
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
    options=[option "--verbose"];
  }

  let main = Docopt.(get int "--verbose")

  let () = 
    Docopt.run main ~doc ~argv:["--verbose"; "--verbose"] (*; "--verbose"]*)
      => Ok 2
end

module Test_russ_cox_pathological_example_with_options = struct
  let _doc = "usage: prog [--verbose]*32 --verbose*32"

  let vo, v = Optional !"--verbose", !"--verbose"
  let vo8 = vo <*> vo <*> vo <*> vo <*> vo <*> vo <*> vo <*> vo 
  let v8 = v <*> v <*> v <*> v <*> v <*> v <*> v <*> v 
  let vo32 = vo8 <*> vo8 <*> vo8 <*> vo8
  let v32 = v8 <*> v8 <*> v8 <*> v8
  let doc = Doc.{
    usage=vo32 <*> v32;
    options=[option "--verbose"];
  }
  let main = Docopt.(get int "--verbose")

  let () = 
    let v = "--verbose" in
    let v8 = [v; v; v; v; v; v; v; v] in
    let argv = v8 @ v8 @ v8 @ v8 in
    Docopt.run main ~doc ~argv
      => Ok 32
end

module Testing_potential_infinite_loop_with_options = struct
  let _doc = "usage: prog [[--verbose]...]..."

  let doc = Doc.{
    usage=Multiple (Optional (Multiple (Optional !"--verbose")));
    options=[option "--verbose"];
  }
  let main = Docopt.(get int "--verbose")

  let () = 
    let v = "--verbose" in
    let v8 = [v; v; v; v; v; v; v; v] in
    let argv = v8 @ v8 @ v8 @ v8 in
    Docopt.run main ~doc ~argv
      => Ok 32
end

module Test_option_unit_bool_int = struct
  let _doc = "usage: prog --unit [--bool] --int..."
  let doc = Doc.{
    usage= !"--unit" <*> Optional !"--bool" <*> Multiple !"--int";
    options=[
      option "--unit"; option "--bool"; option "--int";
    ];
  }

  let main =
    let open Docopt in
    let+ () = get unit "--unit"
    and+ bool = get bool "--bool"
    and+ int = get int "--int" in
    bool, int

  let () =
    match Docopt.run main ~doc ~argv:[] with
    | Error _ -> ()
    | Ok _ -> assert false

  let () =
    Docopt.run main ~doc ~argv:["--unit"; "--bool"; "--int"; "--int"]
      => Ok (true, 2) 

  let () =
    Docopt.run main ~doc ~argv:["--unit"; "--int"]
      => Ok (false, 1)
end

module Test_option_string_option_list = struct
  let _doc = "usage: prog --string=<s> [--option=<o>] --list=<l>..."
  let doc = Doc.{
    usage= !"--string=<s>" <*> Optional !"--option=<o>" <*> Multiple !"--list=<l>";
    options=[
      option "--string" ~argument:true; 
      option "--option" ~argument:true; 
      option "--list" ~argument:true;
    ];
  }

  let main =
    let open Docopt in
    let+ s = get string "--string"
    and+ o = get (option string) "--option"
    and+ l = get (list string) "--list" in
    s, o, l

  let () =
    let argv = ["--string=s"; "--option=o"; "--list=l"; "--list"; "m"] in
    Docopt.run main ~doc ~argv
      => Ok ("s", Some "o", ["l"; "m"]) 

  let () =
    Docopt.run main ~doc ~argv:["--string=s"; "--list=l"]
      => Ok ("s", None, ["l"]) 
end

module Test_short_options = struct
  let _doc = "Usage: prog [--output=<file>...] [--verbose...]
  
  Options:
    -o, --output <file> 
    -v, --verbose
  "
  let doc = Doc.{
    usage=Optional (Multiple !"--output=<file>") <*> Optional (Multiple !"--verbose");
    options=[
      option "--output" ~argument:true ~synonyms:["-o"];
      option "--verbose" ~argument:false ~synonyms:["-v"];
    ];
  }

  let () = Docopt.(run (get int "--verbose")) ~doc ~argv:[] => Ok 0
  let () = Docopt.(run (get int "--verbose")) ~doc ~argv:["-v"] => Ok 1
  let () = Docopt.(run (get int "--verbose")) ~doc ~argv:["-vvv"] => Ok 3
  let () = Docopt.(run (get int "--verbose")) ~doc ~argv:["-v"; "-v"; "-v"] => Ok 3

  let () = Docopt.(run (get (list string) "--output")) ~doc ~argv:[] => Ok []
  let () = Docopt.(run (get (list string) "--output"))
    ~doc ~argv:["-o"; "x"; "-oy"; "-voz"] => Ok ["x"; "y"; "z"]
  let () = Docopt.(run (get (list string) "--output"))
    ~doc ~argv:["-ox"] => Ok ["x"]
  let () = Docopt.(run (get (list string) "--output"))
    ~doc ~argv:["-vox"] => Ok ["x"]

  let main =
    let open Docopt in
    let+ output = get (list string) "--output"
    and+ verbose = get int "--verbose" in
    output, verbose

  let () = 
    Docopt.run main ~doc ~argv:["-vovo"; "-vv"; "-oo"; "-o"; "x"; "-v"]
      => Ok (["vo"; "o"; "x"], 4)
end
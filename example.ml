let doc = "Copy files from source to destination

Usage: 
  cp [-f | --force] [-r | -R | --recursive] [-v | --verbose] <source>... <dest>
  
Options:
  -f, --force           Force it
  -r, -R, --recursive   Recursive
  --help                Show this
  --version             Show version
  
Arguments:
  <source>              Source
  <dest>                Destination
"

let main =
  let open Docopt in
  let+ force = get bool "--force"
  and+ recursive = get bool "--recursive"
  and+ sources = get (list string) "<source>"
  and+ dest = get string "<dest>"
  in
  Printf.printf "Copying files from (%s) to (%s) (recursively: %b, force: %b)"
    (String.concat ", " sources) dest force recursive

let () = Docopt.run_or_exit main ~doc

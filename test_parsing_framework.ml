let (=>) left right = print_string (if left = right then "." else "F")
let raises expected_exn thunk =
  try thunk () |> ignore; 0 => 1
  with exn -> exn => expected_exn

module Source = Parsing_framework.Source
let source = Source.of_string

module TestSource = struct
  Source.to_string (Source.of_string "hai") => "hai"
end

open Parsing_framework

let () = ()
  ; parse any (source "x") => Some 'x'
  ; parse any (source "") => None

let () = ()
  ; parse (satisfy ((=) 'h')) (source "hai") => Some 'h'
  ; parse (satisfy ((=) 'X')) (source "hai") => None

let x = satisfy ((=) 'x')

let () = ()
  ; parse (not_followed_by x) (source "xa") => None
  ; parse (not_followed_by x) (source "aa") => Some ()
  ; parse (not_followed_by x >=> char 'a' >=> end_of_input) (source "a")
      => Some ()
  ; parse (not_followed_by x) (source "") => Some ()
  ; parse (not_followed_by end_of_input) (source "") => None
  ; parse (not_followed_by end_of_input) (source "a") => Some ()
  ; parse (not_followed_by end_of_input >=> char 'a' >=> end_of_input) (source "a")
      => Some ()

let () = ()
  ; parse (char 'x' <|> char 'y') (source "x") => Some 'x'
  ; parse (char 'x' <|> char 'y') (source "y") => Some 'y'
  ; parse (char 'x' <|> char 'x') (source "y") => None
  ; raises (Failure "Ambiguous parse: x") @@ fun () ->
      parse (char 'x' <|> char 'x') (source "x")

let () = ()
  ; parse (char 'x' </> char 'y') (source "x") => Some 'x'
  ; parse (char 'x' </> char 'y') (source "y") => Some 'y'
  ; parse (char 'x' </> char 'x') (source "y") => None
  ; parse (char 'x' </> char 'x') (source "x") => Some 'x'

let () = ()
  ; parse (followed_by x) (source "aa") => None
  ; parse (followed_by x) (source "xa") => Some ()
  (* does not consume input *)
  ; parse (followed_by x >=> char 'x' >=> end_of_input) (source "x")
      => Some ()


let () = ()
  ; parse (zero_or_more x) (source "") => Some []
  ; parse (zero_or_more x) (source "x") => Some ['x']
  ; parse (zero_or_more x) (source "xx") => Some ['x'; 'x']
  ; parse (zero_or_more x) (source "xx") => Some ['x'; 'x']
  ; parse (zero_or_more any) (source "123") => Some ['1'; '2'; '3']

let () = ()
  ; parse (one_or_more x) (source "") => None
  ; parse (one_or_more x) (source "x") => Some ['x']
  ; parse (one_or_more x) (source "xx") => Some ['x'; 'x']
  ; parse (one_or_more x) (source "xx") => Some ['x'; 'x']
  ; parse (one_or_more any) (source "123") => Some ['1'; '2'; '3']

let () = ()
  ; parse (between '2' '4') (source "1") => None
  ; parse (between '2' '4') (source "2") => Some '2'
  ; parse (between '2' '4') (source "3") => Some '3'
  ; parse (between '2' '4') (source "4") => Some '4'
  ; parse (between '2' '4') (source "5") => None

let () = ()
  ; parse (option x) (source "x") => Some (Some 'x')
  ; parse (option x) (source "y") => Some None

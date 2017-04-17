let (=>) left right = print_string (if left = right then "." else "F")

module Source = Parsing.Source
let source = Source.of_string

module TestSource = struct
  Source.to_string (Source.of_string "hai") => "hai"
end

open Parsing

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
  ; parse (not_followed_by x >=> char 'a' >=> eof) (source "a") => Some ()
  ; parse (not_followed_by x) (source "") => Some ()
  ; parse (not_followed_by eof) (source "") => None
  ; parse (not_followed_by eof) (source "a") => Some ()
  ; parse (not_followed_by eof >=> char 'a' >=> eof) (source "a") => Some ()

let () = ()
  ; parse (followed_by x) (source "aa") => None
  ; parse (followed_by x) (source "xa") => Some ()
  (* does not consume input *)
  ; parse (followed_by x >=> char 'x' >=> eof) (source "x") => Some ()

let () = ()
  ; parse (one_or_more x) (source "") => None
  ; parse (one_or_more x) (source "x") => Some ['x']
  ; parse (one_or_more x) (source "xx") => Some ['x'; 'x']
  ; parse (one_or_more x) (source "xx") => Some ['x'; 'x']

let () = ()
  ; parse (between '2' '4') (source "1") => None
  ; parse (between '2' '4') (source "2") => Some '2'
  ; parse (between '2' '4') (source "3") => Some '3'
  ; parse (between '2' '4') (source "4") => Some '4'
  ; parse (between '2' '4') (source "5") => None
